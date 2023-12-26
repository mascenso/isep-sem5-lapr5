namespace UserManagement.Domain.Auth
{
  public class TokenValidationResponseDto(string message)
  {
    public string Message { get; set; } = message;
  }

}

