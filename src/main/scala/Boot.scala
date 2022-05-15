import scala.concurrent.Future

object Boot2 extends App {}

sealed trait FilterModeProto

object FilterModeProto {
  case object Invalid extends FilterModeProto
  case object EQ extends FilterModeProto
  case object GT extends FilterModeProto
  case object LT extends FilterModeProto
  case object LTE extends FilterModeProto
  case object GTE extends FilterModeProto
}

final case class AgeFilterProto(
    age: Int,
    filterMode: FilterModeProto
)

final case class FiltersProto(
    userIds: List[Long],
    ageFilter: Option[AgeFilterProto]
)

final case class UserProto(name: String, address: String, age: Int)
final case class GetUsersReqProto(filters: Option[FiltersProto])
final case class GetUsersRespProto(users: List[UserProto])

trait UserGrpcServiceLike {

  def getUsers(req: GetUsersReqProto): Future[GetUsersRespProto]

}

class UserRepository {
  def getUsers(req: GetUsersReq): List[User] = ???
}

/** Domain models
  */
final case class User(name: String, address: String, age: Int)

sealed trait FilterMode

object FilterMode {
  case object EQ extends FilterMode
  case object GT extends FilterMode
  case object LT extends FilterMode
  case object LTE extends FilterMode
  case object GTE extends FilterMode
}

final case class AgeFilter(
    age: Int,
    filterMode: FilterMode
)

final case class Filters(
    userIds: List[Long],
    ageFilter: Option[AgeFilter]
)

final case class GetUsersReq(filters: Option[Filters])

class UserGrpcService(repo: UserRepository) extends UserGrpcServiceLike {

  override def getUsers(
      req: GetUsersReqProto
  ): Future[GetUsersRespProto] = (for {
    req <- GetUsersValidator.validate(req)
    users = repo.getUsers(req)
    proto = GetUserConverter.toProto(users)
  } yield GetUsersRespProto(proto)) match {
    case Some(value) => Future.successful(value)
    case None        => Future.failed(new Exception())
  }

}

object FilterModeValidator {
  def validate(filterModeProto: FilterModeProto): Option[FilterMode] =
    PartialFunction.condOpt(filterModeProto) {
      case FilterModeProto.EQ  => FilterMode.EQ
      case FilterModeProto.GT  => FilterMode.GT
      case FilterModeProto.LT  => FilterMode.LT
      case FilterModeProto.LTE => FilterMode.LTE
      case FilterModeProto.GTE => FilterMode.GTE
    }
}

object AgeFilterValidator {
  def validate(ageFilter: AgeFilterProto): Option[AgeFilter] = for {
    age <- Option.when(ageFilter.age > 10)(ageFilter.age)
    filterMode <- FilterModeValidator.validate(ageFilter.filterMode)
  } yield AgeFilter(age, filterMode)
}

object FilterValidator {

  type ValidAgeFilter = Option[AgeFilter]

  def validateAge(filter: Option[AgeFilterProto]): Option[ValidAgeFilter] =
    filter match {
      case Some(value) => Some(AgeFilterValidator.validate(value)) // ignore
      case None        => Some(None)
    }

  def validateAge2(filter: Option[AgeFilterProto]): Option[ValidAgeFilter] =
    filter match {
      case Some(value) => AgeFilterValidator.validate(value).map(Some(_))
      case None        => Some(None)
    }

  import cats.syntax.traverse._

  def validate(filtersProto: FiltersProto): Option[Filters] = for {
    ids <- Option.when(filtersProto.userIds.nonEmpty)(filtersProto.userIds)
    ageFilterMaybe <- filtersProto.ageFilter.traverse(
      AgeFilterValidator.validate
    )
  } yield Filters(ids, ageFilterMaybe)
}

object GetUsersValidator {

  def validate(req: GetUsersReqProto): Option[GetUsersReq] = req.filters match {
    case Some(filter) =>
      val filters = FilterValidator.validate(filter)
      Option.when(filters.isDefined)(GetUsersReq(filters))
    case _ => None
  }
}

object GetUserConverter {

  def toProto(users: List[User]): List[UserProto] = users.map { user =>
    UserProto(
      user.name,
      user.address,
      user.age
    )
  }
  def fromProto(users: List[UserProto]): List[User] = users.map { user =>
    User(
      user.name,
      user.address,
      user.age
    )
  }

}

object Boot3 extends App {
  val ageFilterProto1: Option[AgeFilterProto] = Some(
    AgeFilterProto(8, FilterModeProto.GT)
  )
  val ageFilterProto2: Option[AgeFilterProto] = Some(
    AgeFilterProto(12, FilterModeProto.Invalid)
  )
  val ageFilterProto3: Option[AgeFilterProto] = None
  val ageFilterProto4: Option[AgeFilterProto] = Some(
    AgeFilterProto(12, FilterModeProto.EQ)
  )

  println(FilterValidator.validateAge(ageFilterProto1))
  println(FilterValidator.validateAge(ageFilterProto2))
  println(FilterValidator.validateAge(ageFilterProto3))
  println(FilterValidator.validateAge(ageFilterProto4))

  println("\n\n***\n\n")

  println(FilterValidator.validateAge2(ageFilterProto1))
  println(FilterValidator.validateAge2(ageFilterProto2))
  println(FilterValidator.validateAge2(ageFilterProto3))
  println(FilterValidator.validateAge2(ageFilterProto4))

}
