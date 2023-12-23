namespace UserManagement.Domain.Users
{
  public class CreateUserRequestDto(string email, string password, string firstName, string lastName, string role, bool active)
  {
    public string Email { get; set; } = email;
    public string Password { get; set; } = password;
    public string FirstName { get; set; } = firstName;
    public string LastName { get; set; } = lastName;
    public string Role { get; set; } = role;
    public bool Active { get; set; } = active;
  }
}

