using System;

namespace UserManagement.Domain.Shared
{
  public class UserAlreadyRegisteredException : Exception
  {
    public string Details { get; }

    public UserAlreadyRegisteredException(string message) : base(message)
    {

    }

    public UserAlreadyRegisteredException(string message, string details) : base(message)
    {
      this.Details = details;
    }
  }

}

