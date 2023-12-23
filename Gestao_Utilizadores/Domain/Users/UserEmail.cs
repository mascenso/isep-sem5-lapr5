using UserManagement.Domain.Shared;

namespace UserManagement.Domain.Users
{
  public class UserEmail : IValueObject
  {
    public string Value { get; private set; }

    private UserEmail() { }

    public UserEmail(string value)
    {
      this.Value = value;
    }

    //todo add business validations later

  }


}
