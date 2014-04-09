package service

import securesocial.core.Identity
import securesocial.core.providers.Token
import securesocial.core.IdentityId
import play.api.mvc.Controller
import play.modules.reactivemongo.MongoController
import reactivemongo.api.collections.default.BSONCollection
import models.biovertoUser
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import reactivemongo.bson.BSONDocument
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import reactivemongo.bson.BSONDateTime
import org.joda.time.DateTime
import securesocial.core.providers.Token
import securesocial.core.providers.Token
import scala.collection.immutable.Map
import play.api.Application

class Myservice(application:Application) extends securesocial.core.UserServicePlugin(application) with Controller with MongoController {
  
    val collection = db[BSONCollection]("kiran")
    val myTokens   =db[BSONCollection]("tokens")
     private var users = Map[String, Identity]()
  private var tokens = Map[String, Token]()

  def find(id: IdentityId): Option[Identity] = {
   
     findByEmailAndProvider(id.userId,id.providerId)
  }

  def findByEmailAndProvider(email: String, providerId: String): Option[Identity] = {
   val query=BSONDocument("userid"->email,"provider"->providerId)
  
   val cursor  = collection.find(query).cursor[biovertoUser]
   
   val futureuser = cursor.headOption.map{
      case Some(user) => user
      case None => false
    }
    val jobj = Await.result(futureuser, Duration(5,"seconds"))
    jobj match {
      case x : Boolean => None
      case _  => Some(models.biovertoUser.UserBSONReader.read(jobj.asInstanceOf[BSONDocument]))

    }
  
  }

  def save(user: Identity): Identity = {
      val currentUser= biovertoUser.fromIdentity(user)
		    val docToSave = biovertoUser.UserBSONWriter.write(currentUser)
		    collection.insert(docToSave)
    user
  }

  def save(token: Token) {
  //  tokens += (token.uuid -> token)
    val tokendoc= biovertoUser.SecureSocialTokenBSONWriter.write(token)
    myTokens.insert(tokendoc)
  }

  def findToken(token: String): Option[Token] = {
    //tokens.get(token)
    val query = BSONDocument("uuid"->token)
     val cursor  = myTokens.find(query).cursor[BSONDocument]
      val futureuser = cursor.headOption.map{
        case Some(user) => user
        case None => false
     }
      val jobj = Await.result(futureuser, Duration(5,"seconds"))
      
       jobj match {
      case x : Boolean => None
      case _  => Some(models.biovertoUser.SecureSocialTokenBSONReader.read(jobj.asInstanceOf[BSONDocument]))

    }
    
  }

  def deleteToken(uuid: String) {
    val query = BSONDocument("uuid"->uuid)
    myTokens.remove(query)
  }

  

  def deleteExpiredTokens() {
     var query = BSONDocument("expirationTime" -> BSONDocument("$lte" -> BSONDateTime(DateTime.now().getMillis)))
   // myTokens.find(query).cursor[Token].enumerate().map(t => deleteToken(t.uuid))
     val cursor = myTokens.find(query).cursor
     val token= cursor.enumerate().map{
       t=>
         val tokentobedeleted = biovertoUser.SecureSocialTokenBSONReader.read(t)
         deleteToken(tokentobedeleted.uuid)
         
     }
     
  }

}