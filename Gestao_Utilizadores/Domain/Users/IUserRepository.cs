using System.Threading.Tasks;
using System.Collections.Generic;
using UserManagement.Domain.Shared;

namespace UserManagement.Domain.Users
{
  public interface IUserRepository: IRepository<User, UserId>
  {
    Task<User> GetUserByEmailAsync(string email);
    Task<IEnumerable<User>> GetInactiveUsersAsync();
  }
}

