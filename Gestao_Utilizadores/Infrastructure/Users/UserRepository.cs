using System;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.EntityFrameworkCore;
using UserManagement.Domain.Users;
using UserManagement.Infrastructure.Shared;

namespace UserManagement.Infrastructure.Users
{
  public class UserRepository : BaseRepository<User, UserId>, IUserRepository
  {
    private readonly DbSet<User> _objs;

    public UserRepository(UserManagementDbContext context) : base(context.Users)
    {
      this._objs = context.Users;
    }

    public async Task<User> GetUserByEmailAsync(string email)
    {
      // Ensure the email comparison is case-insensitive
      return await this._objs.Where(u => u.Email.ToString().Equals(email, StringComparison.OrdinalIgnoreCase)).FirstOrDefaultAsync();
    }
  }

}

