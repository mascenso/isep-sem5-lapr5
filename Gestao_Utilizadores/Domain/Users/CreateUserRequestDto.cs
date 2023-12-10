namespace UserManagement.Domain.Users
{
  public class CreateUserRequestDto(string email, string firstName, string lastName, string role, bool active)
  {
    public string Email { get; set; } = email;
    public string FirstName { get; set; } = firstName;
    public string LastName { get; set; } = lastName;
    public string Role { get; set; } = role;
    public bool Active { get; set; } = active;
  }
}

