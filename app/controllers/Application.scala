package controllers
import org.joda.time.DateTime
import play.api.Logger
import play.api._
import play.mvc.Result
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.Play.current
import play.api.mvc._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.modules.reactivemongo.{ MongoController, ReactiveMongoPlugin }
import reactivemongo.api.gridfs.GridFS
import reactivemongo.api.gridfs.Implicits.DefaultReadFileReader
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson._
import models.User
import views.html.defaultpages.badRequest
import scala.concurrent.Future
import views.html.users
import models.User
import play.api.libs.iteratee.Iteratee
import play.api.mvc.Action
import models.biovertoUser


object Application extends Controller with securesocial.core.SecureSocial with MongoController
{
  
  val collection = db[BSONCollection]("kiran")
  
		def index = SecuredAction{
		  
		  implicit request =>
		    val currentUser= biovertoUser.fromIdentity(request.user)
		    val docToSave = biovertoUser.UserBSONWriter.write(currentUser)
		  //  collection.insert(docToSave)
		    Ok("User is added to collection kiran")
		    
  
		}/*end of index*/
		
		  def page = UserAwareAction { implicit request =>
    val userName = request.user match {
      case Some(user) => user.fullName
      case _ => "guest"
    }
    Ok("Hello %s".format(userName))
  }

}


object Users extends Controller with MongoController {

	val collection = db[BSONCollection]("users")  // get the collection 'users'
	val gridFS = new GridFS(db)	//a store for collections
	gridFS.ensureIndex().onComplete { // let's build an index on our gridfs chunks collection if none
		case index => Logger.info(s"Checked index, result is $index")
	}	
	def index = Action.async { // list all users and sort them
		implicit request =>
		val sort = getSort(request)     // get a sort document (see getSort method for more information)
		val query = BSONDocument(    // build a selection document with an empty query and a sort subdocument ('$orderby')
			"$orderby" -> sort,
			"$query" -> BSONDocument())
		val activeSort = request.queryString.get("sort").flatMap(_.headOption).getOrElse("none")
		val found = collection.find(query).cursor[User]     // the cursor of documents
		found.collect[List]().map { // build (asynchronously) a list containing all the users
			users => Ok(views.html.users(users, activeSort))
		}.recover {
			case e =>
				e.printStackTrace()
				BadRequest(e.getMessage())
		}
	}
	def showCreationForm = Action {
		Ok(views.html.editUser(None, User.form, None))
	}
	def showEditForm(id: String) = Action.async {
		val objectId = new BSONObjectID(id)
		// get the documents having this id (there will be 0 or 1 result)
		val futureUser = collection.find(BSONDocument("_id" -> objectId)).one[User]
		// ... so we get optionally the matching user, if any
		//let's use for-comprehension to compose futures (see http://doc.akka.io/docs/akka/2.0.3/scala/futures.html#For_Comprehensions for more information)
		for {
			maybeUser <- futureUser // get a future option of user
			result <- maybeUser.map { 		// if there is some user, return a future of result with the user and its attachments
				user => 
					import reactivemongo.api.gridfs.Implicits.DefaultReadFileReader
					// search for the matching attachments
					// find(...).toList returns a future list of documents (here, a future list of ReadFileEntry)
					gridFS.find(BSONDocument("user" -> user.id.get)).collect[List]().map {
						files =>
							val filesWithId = files.map { 
								file => file.id.asInstanceOf[BSONObjectID].stringify -> file
							}
							Ok(views.html.editUser(Some(id), User.form.fill(user), Some(filesWithId)))
					}
			}.getOrElse(Future(NotFound))
		} yield result
	}
	def testme=Action
	{
	  request =>
	    
	    val json= Json.parse(request.body.asText.get) 
	  val Name=(json \ "Name" ).asOpt[String].get
			val graphs=(json \ "graphs" ).asOpt[String].get
		    val username=(json \"username").asOpt[String].get
		   val rjson=Json.obj("username"->username,"status"->"true")
		   Ok(rjson).withSession("username"->username)
	}
	
	def checksession=Action
	{
	  request =>
	    
	    val value=request.session.get("username")
	    if(value.toString().equals("None"))
	    {
	      Ok("login karle bhai!!")
	    }
	    else
	    {
	    Ok("session is "+value.toString())
	    }
	}
	
	def logout= Action
	{
	    val rjson=Json.obj("status"->"successful")
		    	  Ok(rjson).withNewSession  
	}
	
	def create = Action.async {
		implicit request =>
		 try{
		  val json= Json.parse(request.body.asText.get) 
	  val Name=(json \ "Name" ).asOpt[String].get
			val graphs=(json \ "graphs" ).asOpt[String].get
		    val username=(json \"username").asOpt[String].get
		    val id = Option(BSONObjectID.generate)
		    val cdate= Option(new DateTime)
		    val udate =Option(new DateTime)
		    val myuser:User=new  models.User(id,Name,graphs,username,cdate,udate)
		  val bdoc= User.UserBSONWriter.write(myuser)
		    collection.insert(bdoc)
		    val rjson=Json.obj("username"->username,"status"->"true")
		    	  Future.successful(Ok(rjson))  
		  }
	
		 
		 
		 catch{
		    case ex: Exception=>{
		      val rjson=Json.obj("username"->"null","status"->"not valid json")
		        Future.successful(Ok(rjson))  
		    }
		  }
		
		
	}
	/*
	 * To check the user credentials
	 * 
	 */
	def isAuthorized= Action.async
	{
	  implicit request =>
	    var flag=0;
	    
	    try{
      val json= Json.parse(request.body.asText.get).asOpt[JsValue]

    		  
	
     json.map{
	       rjson=>
	          val Name=(rjson \ "Name" ).asOpt[String].get
			val graphs=(rjson \ "graphs" ).asOpt[String].get
		    val username=(rjson \"username").asOpt[String].get
	       
	    val query=BSONDocument("username"->username)
		val found = collection.find(query).cursor[User]     // the cursor of documents
			
			
	     found.collect[List]().map { // build (asynchronously) a list containing all the users
			users => 
			  	for(user<-users)
			  	{
			  	
			  	  if(user.username.equals(username))
			  	  {
			  	     flag=flag+1;
			  	  }
			  	 
			  	}
			 if(flag==1)
			 {
			  
			   val rjson= Json.obj("status"->"true")
			  	Ok("got the user")
			 }
			 else
			 {
			   
			    val rjson= Json.obj("status"->"false")
			  	Ok(rjson)
			 }
		}.recover {
			case e =>
				e.printStackTrace()
				BadRequest(e.getMessage())
		}
		
	     }.getOrElse( Future.successful(Ok("Not valid json")) )
	     
	    }
	    catch
	    {
	      case ex:Exception=>{
	       val rjson=Json.obj("status"->"not valid json")
		        Future.successful(Ok(rjson))  
	      }
	        
	    }
		     
	}
	
	def isDuplicate = Action.async
	{
	  request =>
	      try{
		  val json= Json.parse(request.body.asText.get) 
		    val username=(json \"username").asOpt[String].get
		    val rjson=Json.obj("username"->username,"status"->"true")
		    	  Future.successful(Ok(rjson))  
		  }
		  catch{
		    case ex: Exception=>{
		      val rjson=Json.obj("username"->"null","status"->"not valid json")
		        Future.successful(Ok(rjson))  
		    }
		  }
		
	}
	
	
	def edit(id: String) = Action.async {
		implicit request => User.form.bindFromRequest.fold(
			errors => Future.successful(Ok(views.html.editUser(Some(id), errors, None))),
			user => {
				val objectId = new BSONObjectID(id) // create a modifier document, ie a document that contains the update operations to run onto the documents matching the query
				val modifier = BSONDocument( // this modifier will set the fields 'updateDate', 'title', 'content', and 'publisher'
					"$set" -> BSONDocument(
						"updateDate" -> BSONDateTime(new DateTime().getMillis),
						"Name" -> BSONString(user.Name),
						"graphs" -> BSONString(user.graphs),
						"username" -> BSONString(user.username)
					)
				)
				collection.update(BSONDocument("_id" -> objectId), modifier).map { //ok, let's do the update
					_ => Ok("")
				}
			}
		)
	}
	def delete(id: String) = Action.async {
		//let's collect all the attachments matching that match the user to delete
		gridFS.find(BSONDocument("user" -> new BSONObjectID(id))).collect[List]().flatMap {
			files => // for each attachment, delete their chunks and then their file entry
				val deletions = files.map { file => gridFS.remove(file) }
				Future.sequence(deletions)
		}.flatMap { // now, the last operation: remove the user
			_ => collection.remove(BSONDocument("_id" -> new BSONObjectID(id)))
		}.map(_ => Ok).recover { case _ => InternalServerError }
	}
	// save the uploaded file as an attachment of the user with the given id
	def saveAttachment(id: String) = Action.async(gridFSBodyParser(gridFS)) {
		request => // here is the future file!
			val futureFile = request.body.files.head.ref
			// when the upload is complete, we add the user id to the file entry (in order to find the attachments of the user)
			val futureUpdate = for {
				file <- futureFile
				// here, the file is completely uploaded, so it is time to update the user
				updateResult <- {
					gridFS.files.update(
						BSONDocument("_id" -> file.id),
						BSONDocument("$set" -> BSONDocument("user" -> BSONObjectID(id)))
					)
				}
			} yield updateResult
			futureUpdate.map {
				case _ => Redirect(routes.Users.showEditForm(id))
			}.recover {
				case e => InternalServerError(e.getMessage())
			}
	}
	def getAttachment(id: String) = Action.async { // find the matching attachment, if any, and streams it to the client
		request => val file = gridFS.find(BSONDocument("_id" -> new BSONObjectID(id)))
		request.getQueryString("inline") match {
			case Some("true") => serve(gridFS, file, CONTENT_DISPOSITION_INLINE)
			case _            => serve(gridFS, file)
		}
	}
	def removeAttachment(id: String) = Action.async {
		gridFS.remove(new BSONObjectID(id)).map(_ => Ok).recover { case _ => InternalServerError }
	}
	private def getSort(request: Request[_]) = {
		request.queryString.get("sort").map {
			fields => val sortBy = for {
				order <- fields.map {
					field=> if (field.startsWith("-")) field.drop(1) -> -1
							else field -> 1
				}
				if order._1 == "Name" || order._1 == "username" || order._1 == "creationDate" || order._1 == "updateDate"
			} yield order._1 -> BSONInteger(order._2)
			BSONDocument(sortBy)
		}
	}
}
