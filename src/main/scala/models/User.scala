package models

/**
  * Created by venkatamutyala on 18/03/2017.
  */
case class User(name: String, password: String, id: UserId, role: String)
case class UserId(userId: String)
//case class AdminUser(name: String, password: String, id: String, role: String)
//case class PortfolioUser(name: String, password: String, role: String)
