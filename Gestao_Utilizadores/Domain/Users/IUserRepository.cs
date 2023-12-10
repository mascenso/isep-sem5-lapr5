using UserManagement.Domain.Shared;

namespace UserManagement.Domain.Users
{
  public interface IUserRepository: IRepository<User, UserId>
  {
  }
}

