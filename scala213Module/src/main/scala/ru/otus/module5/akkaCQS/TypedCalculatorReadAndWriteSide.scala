import akka.NotUsed
import akka.actor.typed.{ActorSystem, Behavior, Props}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.persistence.cassandra.query.scaladsl.CassandraReadJournal
import akka.persistence.query.{EventEnvelope, PersistenceQuery}
import akka.persistence.typed.PersistenceId
import akka.persistence.typed.scaladsl.{Effect, EventSourcedBehavior}
import akka.stream.scaladsl.{Broadcast, Flow, GraphDSL, RunnableGraph, Sink, Source}
import akka.stream.{ClosedShape, Graph, Materializer}
import akka_typed.CalculatorRepository.{getLatestOffsetAndResult, initDatabase, updatedResultAndOffset}
import akka_typed.TypedCalculatorWriteSide.{Add, Added, Command, Divide, Divided, Multiplied, Multiply}
import com.typesafe.config.ConfigFactory
import scalikejdbc.{ConnectionPool, ConnectionPoolSettings, DB, using}
import scala.concurrent.ExecutionContext

object akka_typed {

  val persId = PersistenceId.ofUniqueId("001")

  trait CborSerialization

  object TypedCalculatorWriteSide {
    sealed trait Command
    case class Add(amount: Int) extends Command
    case class Multiply(amount: Int) extends Command
    case class Divide(amount: Int) extends Command

    sealed trait Event extends CborSerialization
    case class Added(id: Int, amount: Int) extends Event
    case class Multiplied(id: Int, amount: Int) extends Event
    case class Divided(id: Int, amount: Int) extends Event

    final case class State(value: Double) extends CborSerialization {
      def add(amount: Int): State = copy(value = value + amount)
      def multiply(amount: Int): State = copy(value = value * amount)
      def divide(amount: Int): State = copy(value = value / amount)
    }

    object State {
      val empty = State(0.0)
    }

    def handleCommand(
        persistenceId: String,
        state: State,
        command: Command,
        ctx: ActorContext[Command]
    ): Effect[Event, State] =
      command match {
        case Add(amount) =>
          ctx.log.info(s"receive adding  for number: $amount and state is ${state.value}")
          val added = Added(persistenceId.toInt, amount)
          Effect
            .persist(added)
            .thenRun { x =>
              ctx.log.info(s"The state result is ${x.value}")
            }
        case Multiply(amount) =>
          ctx.log.info(s"receive multiplying  for number: $amount and state is ${state.value}")
          val multiplied = Multiplied(persistenceId.toInt, amount)
          Effect
            .persist(multiplied)
            .thenRun { x =>
              ctx.log.info(s"The state result is ${x.value}")
            }
        case Divide(amount) =>
          ctx.log.info(s"receive dividing  for number: $amount and state is ${state.value}")
          val divided = Divided(persistenceId.toInt, amount)
          Effect
            .persist(divided)
            .thenRun { x =>
              ctx.log.info(s"The state result is ${x.value}")
            }
      }

    def handleEvent(state: State, event: Event, ctx: ActorContext[Command]): State =
      event match {
        case Added(_, amount) =>
          ctx.log.info(s"Handling event Added is: $amount and state is ${state.value}")
          state.add(amount)
        case Multiplied(_, amount) =>
          ctx.log.info(s"Handling event Multiplied is: $amount and state is ${state.value}")
          state.multiply(amount)
        case Divided(_, amount) =>
          ctx.log.info(s"Handling event Divided is: $amount and state is ${state.value}")
          state.divide(amount)
      }

    def apply(): Behavior[Command] =
      Behaviors.setup { ctx =>
        EventSourcedBehavior[Command, Event, State](
          persistenceId = persId,
          State.empty,
          (state, command) => handleCommand("001", state, command, ctx),
          (state, event) => handleEvent(state, event, ctx)
        )
      }
  }

  // Result case class for state and offset
  case class Result(state: Double, offset: Long)

  case class TypedCalculatorReadSide(system: ActorSystem[NotUsed]) {
    initDatabase

    // Add explicit types for implicit values
    implicit val materializer: Materializer = Materializer(system.classicSystem)
    implicit val ec: ExecutionContext = system.executionContext

    // Get latest offset and result
    val (offset, latestCalculatedResult) = getLatestOffsetAndResult
    val startOffset = if (offset == 1) 1 else offset + 1

    val readJournal = PersistenceQuery(system).readJournalFor[CassandraReadJournal](
      CassandraReadJournal.Identifier
    )

    // Function to update state based on events
    def updateState(event: Any, currentState: Double): Double = {
      event match {
        case Added(_, amount) =>
          currentState + amount
        case Multiplied(_, amount) =>
          currentState * amount
        case Divided(_, amount) =>
          currentState / amount
      }
    }

    // Create the source from persistence query
    val source: Source[EventEnvelope, NotUsed] =
      readJournal.eventsByPersistenceId("001", startOffset, Long.MaxValue)

    // Flow to process events and update state
    val stateUpdateFlow: Flow[EventEnvelope, Result, NotUsed] = 
      Flow[EventEnvelope].statefulMapConcat { () =>
        var currentState = latestCalculatedResult
        
        { envelope =>
          val newState = updateState(envelope.event, currentState)
          currentState = newState
          List(Result(newState, envelope.sequenceNr))
        }
    }

    // Sink for local logging - use .mapMaterializedValue to convert to NotUsed
    val localLogSink: Sink[Result, NotUsed] = Sink.foreach[Result] { result =>
      println(s"Local log - State: ${result.state}, Offset: ${result.offset}")
    }.mapMaterializedValue(_ => NotUsed)

    // Sink for database persistence - use .mapMaterializedValue to convert to NotUsed
    val dbSink: Sink[Result, NotUsed] = Sink.foreach[Result] { result =>
      updatedResultAndOffset(result.state, result.offset)
      println(s"Saved to DB - State: ${result.state}, Offset: ${result.offset}")
    }.mapMaterializedValue(_ => NotUsed)

    // Build the graph using GraphDSL
    val graph: Graph[ClosedShape, NotUsed] = GraphDSL.create() { implicit builder =>
      import GraphDSL.Implicits._

      // Source
      val input = builder.add(source)
      
      // State update flow
      val stateUpdater = builder.add(stateUpdateFlow)
      
      // Broadcast to split the stream
      val broadcast = builder.add(Broadcast[Result](2))
      
      // Sinks
      val localSink = builder.add(localLogSink)
      val databaseSink = builder.add(dbSink)

      // Connect the graph
      input ~> stateUpdater ~> broadcast ~> localSink
                       broadcast ~> databaseSink

      ClosedShape
    }

    // Run the graph
    RunnableGraph.fromGraph(graph).run()
  }

  object CalculatorRepository {
    def initDatabase(): Unit = {
      val poolSettings = ConnectionPoolSettings(initialSize = 10, maxSize = 100)
      ConnectionPool.singleton("jdbc:postgresql://localhost:5432/demo", "docker", "docker", poolSettings)
    }

    def getLatestOffsetAndResult: (Long, Double) = {
      val entities = DB readOnly { session =>
        session.list("select * from public.result where id = 1;") { row =>
          (
            row.long("write_side_offset"),
            row.double("calculated_value")
          )
        }
      }
      entities.headOption.getOrElse((0L, 0.0))
    }

    def updatedResultAndOffset(calculated: Double, offset: Long): Unit = {
      using(DB(ConnectionPool.borrow())) { db =>
        db.autoClose(true)
        db.localTx { session =>
          session.update(
            "update public.result set calculated_value = ?, write_side_offset = ? where id = 1",
            calculated,
            offset
          )
        }
      }
    }
  }

  def apply(): Behavior[NotUsed] =
    Behaviors.setup { ctx =>
      val writeActorRef = ctx.spawn(TypedCalculatorWriteSide(), "Calc", Props.empty)
      writeActorRef ! Add(10)
      writeActorRef ! Multiply(2)
      writeActorRef ! Divide(5)

      Behaviors.same
    }

  def main(args: Array[String]): Unit = {
    val value = akka_typed()
    val config = ConfigFactory.load("application.conf")

    implicit val system: ActorSystem[NotUsed] = ActorSystem(value, "akka_typed", config)
    TypedCalculatorReadSide(system)
  }
}
