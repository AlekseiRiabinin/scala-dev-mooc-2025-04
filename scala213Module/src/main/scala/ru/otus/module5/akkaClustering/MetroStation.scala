package ru.otus.module5.akkaClustering
import akka.actor.Props
import akka.cluster.sharding.{ClusterSharding, ClusterShardingSettings, ShardRegion}
import com.typesafe.config.ConfigFactory
import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem}

import scala.util.Random
import java.util.{Date, UUID}

trait CborSerializable
case class TroykaCard(id: String, isAllowed: Boolean) extends  CborSerializable
case class EntryAttemp(trioykaCard: TroykaCard, date: Date) extends  CborSerializable
case class EntryRejected(reason: String) extends  CborSerializable
case object EntryAccepted extends  CborSerializable

class Turnstile(validator: ActorRef) extends Actor with ActorLogging {
  override def receive: Receive = {
    case o: TroykaCard =>
      log.info("validator")
      validator ! EntryAttemp(o, new Date)
    case EntryAccepted => log.info("Green")
    case EntryRejected(reason) => log.info(s"Red $reason")
  }
}


class TroykaCovidPassValidator extends  Actor with ActorLogging {
  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    super.preStart()
    log.info("start checking")
  }

  override def receive: Receive = {
    case EntryAttemp(card@ TroykaCard(_, isAllowed), _) =>
      log.info(s"validation $card")
      if (isAllowed) sender() ! EntryAccepted
      else sender() ! EntryRejected("not your day, sorry")
  }
}

object TurnstileSettings {
  val numberOfShards = 3
  val numberOfEntites = 30

  val extarctEntityId: ShardRegion.ExtractEntityId = {
    case attemp @ EntryAttemp(TroykaCard(cardId, _), _) =>
      val entryId = cardId.hashCode % numberOfEntites
      println(s"!!!!! extract entry id for card # ${attemp.trioykaCard.id} to entry ID ${entryId}" )
      (entryId.toString, attemp)
  }

  val extractShardId: ShardRegion.ExtractShardId = {
    case EntryAttemp(TroykaCard(cardId, _), _) =>
      val shardId = cardId.hashCode % numberOfShards
      println(s"!!!!! extract shard id for card # ${cardId} to shard ID ${shardId}" )
      shardId.toString
  }
}

class MetroStation(port: Int, amountOfTurnstile: Int) extends App {
  val config = ConfigFactory.parseString(
    s"""
       akka.remote.artery.canonical.port = $port
       |""".stripMargin).withFallback(ConfigFactory.load("clusterShardingExample.conf"))

  val system = ActorSystem("DemoCluster", config)

  val validatorShardRegionRef: ActorRef =
    ClusterSharding(system).start(
      typeName = "TroykaCovidPassValidator",
      entityProps = Props[TroykaCovidPassValidator],
      settings = ClusterShardingSettings(system),
      extractEntityId = TurnstileSettings.extarctEntityId,
      extractShardId = TurnstileSettings.extractShardId
    )

  val turnstiles: Seq[ActorRef] = (1 to amountOfTurnstile)
    .map{x=>
      println(s"Before starting actor of turnstiles # $x")
      system.actorOf(Props(new Turnstile(validatorShardRegionRef)))
    }

  Thread.sleep(3000)

  for (_ <- 1 to 1000) {
    val randomTurnStileIndex = Random.nextInt(amountOfTurnstile)
    val randomTurnstile = turnstiles(randomTurnStileIndex)

    randomTurnstile ! TroykaCard(UUID.randomUUID().toString, Random.nextBoolean())
    Thread.sleep(200)
  }
}


object ChistyePrude extends MetroStation(2551, 10)
object Lubanka extends MetroStation(2562, 5)
object OkhotnuRad extends MetroStation(2563, 15)