// http://stackoverflow.com/questions/31949455/transform-one-case-class-into-another-when-the-argument-list-is-the-same

object User {
  case class Create(userName:String, firstName: String, lastName: String)
  case class Created(userName:String, firstName: String, lastName: String)
}

object Group {
  case class Create(groupName:String, members: Int)
  case class Created(groupName:String, members: Int)
}
