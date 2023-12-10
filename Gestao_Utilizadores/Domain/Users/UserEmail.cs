using System;
using UserManagement.Domain.Shared;

namespace UserManagement.Domain.Users
{
  public class UserEmail : IValueObject
  {
    private string _value;

    private UserEmail() { }

    public UserEmail(string value)
    {
      this._value = value;
    }

    public string Value()
    {
      return this._value;
    }

    private void SetValue(string value)
    {
      this._value = value;
    }

    //todo add business validations later

  }


}
