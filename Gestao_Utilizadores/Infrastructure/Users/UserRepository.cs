﻿using System;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.EntityFrameworkCore;
using UserManagement.Domain.Users;
using UserManagement.Infrastructure.Shared;
using System.Collections.Generic;


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
      return await this._objs.FirstOrDefaultAsync(u => EF.Functions.Like(u.Email.Value, email));
    }

    public async Task<IEnumerable<User>> GetInactiveUsersAsync()
    {
        return await this._objs.Where(u => !u.Active).ToListAsync();
    }

  }

}
