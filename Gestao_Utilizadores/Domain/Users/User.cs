using System;
using UserManagement.Domain.Shared;

namespace UserManagement.Domain.Users
{
  public class User : Entity<UserId>, IAggregateRoot
  {
    private UserEmail _email;

    public UserPassword _password;

    private string _firstName;

    private string _lastName;

    private string _role;

    private bool _active;

    private User()
    {
      this._active = true;
    }

    public User(UserEmail email, UserPassword password, string firstName, string lastName, string role, bool active)
    {
      this.Id = new UserId(Guid.NewGuid());
      this._email = email;
      this._password = password;
      this._firstName = firstName;
      this._lastName = lastName;
      this._role = role;
      this._active = active;
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
