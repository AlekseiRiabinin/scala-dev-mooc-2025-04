package ru.otus.module4.homework.dao.repository


import zio.{ULayer, ZIO, ZLayer}
import io.getquill.context.ZioJdbc._
import ru.otus.module4.homework.dao.entity._
import ru.otus.module4.phoneBook.db

import java.sql.SQLException
import javax.sql.DataSource


trait UserRepository{
    def findUser(userId: UserId): QIO[Option[User]]
    def createUser(user: User): QIO[User]
    def createUsers(users: List[User]): QIO[List[User]]
    def updateUser(user: User): QIO[Unit]
    def deleteUser(user: User): QIO[Unit]
    def findByLastName(lastName: String): QIO[List[User]]
    def list(): QIO[List[User]]
    def userRoles(userId: UserId): QIO[List[Role]]
    def insertRoleToUser(roleCode: RoleCode, userId: UserId): QIO[Unit]
    def listUsersWithRole(roleCode: RoleCode): QIO[List[User]]
    def findRoleByCode(roleCode: RoleCode): QIO[Option[Role]]
}


class UserRepositoryImpl extends UserRepository {
    val dc = db.Ctx
    import dc._

    private val users = quote(querySchema[User]("users"))
    private val roles = quote(querySchema[Role]("roles"))
    private val userToRoles = quote(querySchema[UserToRole]("user_to_role"))

    override def findUser(userId: UserId): QIO[Option[User]] = 
        run(users.filter(_.id == lift(userId.id))).map(_.headOption)

    override def createUser(user: User): QIO[User] = 
        run(users.insertValue(lift(user))).as(user)

    override def createUsers(usersList: List[User]): QIO[List[User]] = 
        run(liftQuery(usersList).foreach(user => users.insertValue(user))).as(usersList)

    override def updateUser(user: User): QIO[Unit] = 
        run(users.filter(_.id == lift(user.id)).updateValue(lift(user))).unit

    override def deleteUser(user: User): QIO[Unit] = 
        run(users.filter(_.id == lift(user.id)).delete).unit

    override def findByLastName(lastName: String): QIO[List[User]] = 
        run(users.filter(_.lastName == lift(lastName)))

    override def list(): QIO[List[User]] = 
        run(users)

    override def userRoles(userId: UserId): QIO[List[Role]] = 
        run(
            for {
                utr <- userToRoles if utr.userId == lift(userId.id)
                r <- roles if r.code == utr.roleId
            } yield r
        )

    override def insertRoleToUser(roleCode: RoleCode, userId: UserId): QIO[Unit] = 
        run(userToRoles.insertValue(lift(UserToRole(roleCode.code, userId.id)))).unit

    override def listUsersWithRole(roleCode: RoleCode): QIO[List[User]] = 
        run(
            for {
                utr <- userToRoles if utr.roleId == lift(roleCode.code)
                u <- users if u.id == utr.userId
            } yield u
        )

    override def findRoleByCode(roleCode: RoleCode): QIO[Option[Role]] = 
        run(roles.filter(_.code == lift(roleCode.code))).map(_.headOption)
}

object UserRepository {
    val layer: ULayer[UserRepository] = ZLayer.succeed(new UserRepositoryImpl)
}
