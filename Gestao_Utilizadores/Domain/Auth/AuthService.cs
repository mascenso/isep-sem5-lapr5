using System;
using System.IdentityModel.Tokens.Jwt;
using System.Security.Claims;
using System.Text;
using Microsoft.Extensions.Configuration;
using Microsoft.IdentityModel.Tokens;
using UserManagement.Domain.Users;

namespace UserManagement.Domain.Auth
{
  public class AuthService
  {
    private readonly JwtSettings _jwtSettings;

    public AuthService(JwtSettings jwtSettings)
    {
      this._jwtSettings = jwtSettings;
    }

    public string GenerateJwtToken(UserDto userDto)
    {
      var claims = new[]
      {
        new Claim(JwtRegisteredClaimNames.Sub, userDto.Id.ToString()),
        new Claim(JwtRegisteredClaimNames.Email, userDto.Email),
        new Claim(JwtRegisteredClaimNames.GivenName, userDto.FirstName),
        new Claim(JwtRegisteredClaimNames.FamilyName, userDto.LastName),
        new Claim("role", userDto.Role),
        new Claim(JwtRegisteredClaimNames.Jti, Guid.NewGuid().ToString())

      };

      var key = new SymmetricSecurityKey(Encoding.UTF8.GetBytes(this._jwtSettings.SecretKey));
      var credentials = new SigningCredentials(key, SecurityAlgorithms.HmacSha256);
      var expires = DateTime.Now.AddMinutes(this._jwtSettings.ExpiryMinutes); // Token expiry time

      var token = new JwtSecurityToken(
        this._jwtSettings.Issuer,
        this._jwtSettings.Audience,
        claims,
        expires: expires,
        signingCredentials: credentials
      );

      return new JwtSecurityTokenHandler().WriteToken(token);
    }
  }
}

