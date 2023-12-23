using System;
using System.Security.Cryptography;
using System.Security.Policy;
using System.Text;
using UserManagement.Domain.Shared;


namespace UserManagement.Domain.Users
{
  public class UserPassword : IValueObject
  {
    public string Value { get; private set; }
    private UserPassword() { }

    public UserPassword(string value)
    {
      this.Value = value;
    }

    public bool VerifyPassword(string password)
    {
      return BCrypt.Net.BCrypt.Verify(password, this.Value);
    }

    public static string HashPassword(string password)
    {
      return BCrypt.Net.BCrypt.HashPassword(password, BCrypt.Net.BCrypt.GenerateSalt());
    }

    // add validation rules (password length etc)

  }
}
