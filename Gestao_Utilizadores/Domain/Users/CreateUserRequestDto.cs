namespace UserManagement.Domain.Users
{
  public class CreateUserRequestDto(string email, string password, string firstName, string lastName, string role, bool active, string taxPayerNumber, string mechanographicNumber, string phoneNumber)
  {
    public string Email { get; set; } = email;
    public string Password { get; set; } = password;
    public string FirstName { get; set; } = firstName;
    public string LastName { get; set; } = lastName;
    public string Role { get; set; } = role;
    public bool Active { get; set; } = active;
    public string TaxPayerNumber { get; set; } = taxPayerNumber;
    public string MechanographicNumber { get; set; } = mechanographicNumber;
    public string PhoneNumber { get; set; } = phoneNumber;
  }
}

