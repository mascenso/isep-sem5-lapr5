namespace UserManagement.Domain.Auth
{
  public class TokenDto(string accessToken, int expiresIn)
  {
    public string AccessToken { get; set; } = accessToken;
    public int ExpiresIn { get; set; } = expiresIn;
  }

}

