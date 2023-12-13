namespace UserManagement.Domain.Users
{
  public class UserLoginResponseDto
  {
    public string Token { get; set; }
    public UserDto UserDto { get; set; }
  }
}

