using System;

namespace UserManagement.Domain.Users
{
  public class UserDto(Guid id, string email, string firstName, string lastName, string role, bool active)
  {
    public Guid Id { get; set; } = id;
    public string Email { get; set; } = email;
    public string FirstName { get; set; } = firstName;
    public string LastName { get; set; } = lastName;
    public string Role { get; set; } = role;
    public bool Active { get; set; } = active;
  }
}

