using System;
namespace UserManagement.Domain.Users
{
  public class UpdateUserRequestDto
  {
    public Guid id { get; set; }
    public string email { get; set; }
    public string FirstName { get; set; }
    public string LastName { get; set; }
    public string role { get; set; }
    public bool active { get; set; }
  }
}
