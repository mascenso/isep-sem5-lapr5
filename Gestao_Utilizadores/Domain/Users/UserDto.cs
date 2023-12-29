using System;

namespace UserManagement.Domain.Users
{
  public class UserDto(Guid id, string email, string firstName, string lastName, string role, bool active, string taxPayerNumber, string mechanographicNumber, string phoneNumber)
  {
    public Guid Id { get; set; } = id;
    public string Email { get; set; } = email;
    public string FirstName { get; set; } = firstName;
    public string LastName { get; set; } = lastName;
    public string Role { get; set; } = role;
    public bool Active { get; set; } = active;
    public string TaxPayerNumber { get; set; } = taxPayerNumber;
    public string MechanographicNumber { get; set; } = mechanographicNumber;
    public string PhoneNumber { get; set; } = phoneNumber;
  }
}

