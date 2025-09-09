package ru.otus.module4.homework.services


import io.getquill.context.ZioJdbc.QIO
import ru.otus.module4.homework.dao.entity.{Role, RoleCode, User}
import ru.otus.module4.homework.dao.repository.UserRepository
import ru.otus.module4.phoneBook.db
import zio.{ZIO, ZLayer}
import java.sql.SQLException


trait UserService{
    def listUsers(): QIO[List[User]]
    def listUsersDTO(): QIO[List[UserDTO]]
    def addUserWithRole(user: User, roleCode: RoleCode): QIO[UserDTO]
    def listUsersWithRole(roleCode: RoleCode): QIO[List[UserDTO]]
}

class Impl(userRepo: UserRepository) extends UserService {
    val dc = db.Ctx
    import dc._

    def listUsers(): QIO[List[User]] =
        userRepo.list()

    def listUsersDTO(): QIO[List[UserDTO]] = 
        for {
            allUsers <- userRepo.list()
            usersWithRoles <- ZIO.foreach(allUsers) { user =>
                userRepo.userRoles(user.typedId).map(roles => UserDTO(user, roles.toSet))
            }
        } yield usersWithRoles

    def addUserWithRole(user: User, roleCode: RoleCode): QIO[UserDTO] = {
        val dc = db.Ctx
        import dc._
        import javax.sql.DataSource

        // Convert all operations to QIO explicitly
        val createUserOp: QIO[Unit] = userRepo.createUser(user).unit
        val insertRoleOp: QIO[Unit] = userRepo.insertRoleToUser(roleCode, user.typedId)
        val findRoleOp: QIO[Role] = userRepo.findRoleByCode(roleCode).flatMap {
            case Some(role) => ZIO.succeed(role)
            case None => ZIO.fail(new SQLException(s"Role ${roleCode.code} not found"))
        }
        val getUserRolesOp: QIO[List[Role]] = userRepo.userRoles(user.typedId)
        
        // Wrap the entire computation and convert errors to SQLException
        val transactionEffect: ZIO[DataSource, Throwable, UserDTO] = transaction {
            for {
                _ <- createUserOp
                _ <- insertRoleOp
                role <- findRoleOp
                userRoles <- getUserRolesOp
            } yield UserDTO(user, userRoles.toSet + role)
        }
        
        // Convert the error type from Throwable to SQLException
        transactionEffect.mapError {
            case e: SQLException => e
            case e: Throwable => new SQLException(e.getMessage, e)
        }
    }

    def listUsersWithRole(roleCode: RoleCode): QIO[List[UserDTO]] = 
        for {
            users <- userRepo.listUsersWithRole(roleCode)
            usersWithRoles <- ZIO.foreach(users) { user =>
                userRepo.userRoles(user.typedId).map(roles => UserDTO(user, roles.toSet))
            }
        } yield usersWithRoles
}

object UserService {
    val layer: ZLayer[UserRepository, Nothing, UserService] = 
        ZLayer.fromFunction(new Impl(_))
}

case class UserDTO(user: User, roles: Set[Role])
