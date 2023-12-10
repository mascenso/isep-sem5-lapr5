using UserManagement.Domain.Users;
using UserManagement.Infrastructure.Shared;

namespace UserManagement.Infrastructure.Users
{
  public class UserRepository : BaseRepository<User, UserId>, IUserRepository
  {
    public UserRepository(UserManagementDbContext context) : base(context.Users)
    {
    }
  }

}

