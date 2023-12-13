using System.Threading.Tasks;
using UserManagement.Domain.Shared;

namespace UserManagement.Domain.Users
{
  public interface IUserRepository: IRepository<User, UserId>
  {
    Task<User> GetUserByEmailAsync(string email);
  }
}

