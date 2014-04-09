package models
import org.jboss.netty.buffer._
import org.joda.time.DateTime
import play.api.data._
import play.api.data.Forms._
import play.api.data.format.Formats._
import play.api.data.validation.Constraints._
import reactivemongo.bson._
import securesocial.core.AuthenticationMethod
import securesocial.core.OAuth1Info
import securesocial.core.OAuth2Info
import securesocial.core.PasswordInfo
import securesocial.core.Identity
import securesocial.core.IdentityId
import play.api.libs.Codecs
import securesocial.core.providers.Token
case class User(
	id: Option[BSONObjectID],
	Name: String,
	graphs: String,
	username: String,
	creationDate: Option[DateTime],
	updateDate: Option[DateTime])
	// Turn off your mind, relax, and float downstream
	// It is not dying...
	object User {
		implicit object UserBSONReader extends BSONDocumentReader[User] {
			def read(doc: BSONDocument): User =
				User(
					doc.getAs[BSONObjectID]("_id"),
					doc.getAs[String]("Name").get,
					doc.getAs[String]("graphs").get,
					doc.getAs[String]("username").get,
					doc.getAs[BSONDateTime]("creationDate").map(dt => new DateTime(dt.value)),
					doc.getAs[BSONDateTime]("updateDate").map(dt => new DateTime(dt.value))
				)
		}

		implicit object UserBSONWriter extends BSONDocumentWriter[User] {
			def write(user: User): BSONDocument =
				BSONDocument(
					"_id" -> user.id.getOrElse(BSONObjectID.generate),
					"Name" -> user.Name,
					"graphs" -> user.graphs,
					"username" -> user.username,
					"creationDate" -> user.creationDate.map(date => BSONDateTime(date.getMillis)),
					"updateDate" -> user.updateDate.map(date => BSONDateTime(date.getMillis))
				)
		}

		val form = Form(
			mapping(
				"id" -> optional(of[String] verifying pattern(
					"""[a-fA-F0-9]{24}""".r,
					"constraint.objectId",
					"error.objectId")),
				"Name" -> nonEmptyText,
				"graphs" -> text,
				"username" -> nonEmptyText,
				"creationDate" -> optional(of[Long]),
				"updateDate" -> optional(of[Long])
			) { (id, Name, graphs, username, creationDate, updateDate) =>
					User(
						id.map(new BSONObjectID(_)),
						Name,
						graphs,
						username,
						creationDate.map(new DateTime(_)),
						updateDate.map(new DateTime(_))
					)
			} { user => Some(
					(user.id.map(_.stringify),
					user.Name,
					user.graphs,
					user.username,
					user.creationDate.map(_.getMillis),
					user.updateDate.map(_.getMillis))
				)
			}
		)
}




case class biovertoUser(pid: Option[BSONObjectID] = None,
                userId: String,
                providerId: String,
                email: Option[String],
                firstName: String,
                lastName: String,
                authMethod: AuthenticationMethod,
                oAuth1Info: Option[OAuth1Info] = None,
                oAuth2Info: Option[OAuth2Info] = None,
                passwordInfo: Option[PasswordInfo] = None)
                 extends Identity {
  def identityId: IdentityId = IdentityId(userId, providerId)

  def fullName: String = s"$firstName $lastName"

  def avatarUrl: Option[String] = email.map {
    e => s"http://www.gravatar.com/avatar/${Codecs.md5(e.getBytes)}.png"
  }
}
                
                
                
object biovertoUser {

  /* *******************************************************************************************************************
  AuthenticationMethod serialization
  */

  implicit object AuthenticationMethodBSONWriter extends BSONWriter[securesocial.core.AuthenticationMethod, BSONValue] {
    def write(authMethod: AuthenticationMethod) = BSONDocument("authMethod" -> authMethod.method)
  }

  implicit object AuthenticationMethodBSONReader extends BSONReader[BSONValue, securesocial.core.AuthenticationMethod] {
    def read(value: BSONValue) = {
      value match {
        case s: BSONString => securesocial.core.AuthenticationMethod(s.value)
        case _ => securesocial.core.AuthenticationMethod("")
      }
    }
  }

  /* *******************************************************************************************************************
  OAuth1Info serialization
  */

  implicit object OAuth1InfoBSONWriter extends BSONWriter[securesocial.core.OAuth1Info, BSONValue] {
    def write(oauth: OAuth1Info) = BSONDocument("secret" -> oauth.secret, "token" -> oauth.token)
  }

  implicit object OAuth1InfoBSONReader extends BSONReader[BSONValue, securesocial.core.OAuth1Info] {
    def read(value: BSONValue) = {
      value match {
        case doc: BSONDocument => OAuth1Info(doc.getAs[String]("token").get, doc.getAs[String]("secret").get)
        case _ => OAuth1Info("", "")
      }
    }
  }

  /* *******************************************************************************************************************
  OAuth2Info serialization
  */

  implicit object OAuth2InfoBSONWriter extends BSONWriter[securesocial.core.OAuth2Info, BSONValue] {
    def write(oauth: OAuth2Info) = BSONDocument(
      "accessToken" -> oauth.accessToken,
      "tokenType" -> oauth.tokenType,
      "expiresIn" -> oauth.expiresIn,
      "refreshToken" -> oauth.refreshToken)
  }

  implicit object OAuth2InfoBSONReader extends BSONReader[BSONValue, securesocial.core.OAuth2Info] {
    def read(value: BSONValue) = {
      value match {
        case doc: BSONDocument => OAuth2Info(
          doc.getAs[String]("accessToken").get,
          doc.getAs[String]("tokenType"),
          doc.getAs[Int]("expiresIn"),
          doc.getAs[String]("refreshToken"))
        case _ => OAuth2Info("", Option(""))
      }
    }
  }

  /* *******************************************************************************************************************
  PasswordInfo serialization
  */

  implicit object PasswordInfoBSONWriter extends BSONWriter[securesocial.core.PasswordInfo, BSONValue] {
    def write(pi: PasswordInfo) = BSONDocument(
      "hasher" -> pi.hasher,
      "password" -> pi.password,
      "salt" -> pi.salt)
  }

  implicit object PasswordInfoBSONReader extends BSONReader[BSONValue, securesocial.core.PasswordInfo] {
    def read(value: BSONValue) = {
      value match {
        case doc: BSONDocument => PasswordInfo(
          doc.getAs[String]("hasher").get,
          doc.getAs[String]("password").get,
          doc.getAs[String]("salt"))
        case _ => PasswordInfo("", "")
      }
    }
  }

  implicit object UserBSONWriter extends BSONDocumentWriter[biovertoUser] {
    def write(user: biovertoUser) = {
      BSONDocument(
        "_id" -> user.pid,
        "userId" -> user.identityId.userId,
        "providerId" -> user.identityId.providerId,
        "email" -> user.email,
        "firstName" -> user.firstName,
        "lastName" -> user.lastName,
        "authMethod" -> user.authMethod,
        "oAuth1Info" -> user.oAuth1Info,
        "oAuth2Info" -> user.oAuth2Info,
        "passwordInfo" -> user.passwordInfo
      )
    }
  }


 implicit object UserBSONReader extends BSONDocumentReader[biovertoUser] {
    def read(doc: BSONDocument): biovertoUser = {
      biovertoUser(
        doc.getAs[BSONObjectID]("_id"),
        doc.getAs[String]("userId").get,
        doc.getAs[String]("providerId").get,
        doc.getAs[String]("email"),
        doc.getAs[String]("firstName").get,
        doc.getAs[String]("lastName").get,
        doc.getAs[AuthenticationMethod]("authMethod").get,
        doc.getAs[OAuth1Info]("oAuth1Info"),
        doc.getAs[OAuth2Info]("oAuth2Info"),
        doc.getAs[PasswordInfo]("passwordInfo")
      )
    }
  }

  def fromIdentity(user: Identity) = {
    biovertoUser(
      pid = None,
      userId = user.identityId.userId,
      providerId = user.identityId.providerId,
      email = user.email,
      firstName = user.firstName,
      lastName = user.lastName,
      authMethod = user.authMethod,
      oAuth1Info = user.oAuth1Info,
      oAuth2Info = user.oAuth2Info,
      passwordInfo = user.passwordInfo
    )
  }
  
  
  /*
   * For the Token reading
   * */
  
  implicit object BSONDateTimeReader extends reactivemongo.bson.BSONReader[reactivemongo.bson.BSONValue, org.joda.time.DateTime] {
    def read(dateTime: BSONValue) = {
      dateTime match {
        case dt: BSONDateTime => new DateTime(dt.value)
        case _ => new DateTime()
      }
    }
  }
  
  implicit object JodaDateTimeWriter extends reactivemongo.bson.BSONWriter[org.joda.time.DateTime, reactivemongo.bson.BSONValue] {
    def write(dateTime: DateTime) = BSONDateTime(dateTime.getMillis)
  }
  
  implicit object SecureSocialTokenBSONWriter extends BSONDocumentWriter[securesocial.core.providers.Token] {
    def write(token: Token) = {
      BSONDocument(
        "uuid" -> token.uuid,
        "email" -> token.email,
        "creationTime" -> token.creationTime,
        "expirationTime" -> token.expirationTime,
        "isSignUp" -> token.isSignUp
      )
    }
  }
  
    implicit object SecureSocialTokenBSONReader extends BSONDocumentReader[Token] {
    def read(doc: BSONDocument): Token = {
      Token(
        doc.getAs[String]("uuid").get,
        doc.getAs[String]("email").get,
        doc.getAs[DateTime]("creationTime").get,
        doc.getAs[DateTime]("expirationTime").get,
        doc.getAs[Boolean]("isSignUp").get
      )
    }
  }
  
}//end of biovertoUser Object


