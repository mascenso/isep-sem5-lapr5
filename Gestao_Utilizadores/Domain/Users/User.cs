using Microsoft.AspNetCore.Identity;
using System;
using UserManagement.Domain.Shared;
using UserManagement.Domain.Users;

namespace UserManagement.Domain.Users
{
  public class User : Entity<UserId>, IAggregateRoot
  {
    public UserEmail Email { get; private set; }

    public UserPassword Password { get; private set; }

    public string FirstName { get; private set; }

    public string LastName { get; private set; }

    public string Role { get; private set; }

    public bool Active { get; private set; }

    private User()
    {
      this.Active = true;
    }

    public User(UserEmail Email, UserPassword UserPassword, string FirstName, string LastName, string Role, bool Active)
    {
      this.Id = new UserId(Guid.NewGuid());
      this.Email = Email;
      this.Password = UserPassword;
      this.FirstName = FirstName;
      this.LastName = LastName;
      this.Role = Role;
      this.Active = Active;
    }

    //public void ChangeDescription(string description)
    //{
    //  if (!this.Active)
    //    throw new BusinessRuleValidationException("It is not possible to change the description of an inactive user.");
    //  this.Description = description;
    //}

    //public void MarkAsInative()
    //{
    //  this.Active = false;
    //}
  }
}
