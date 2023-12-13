using System;
using System.IdentityModel.Tokens.Jwt;
using System.Security.Claims;
using System.Text;
using System.Threading.Tasks;
using Microsoft.IdentityModel.Tokens;
using UserManagement.Domain.Shared;
using UserManagement.Domain.Users;
using UserManagement.Mappers;

namespace UserManagement.Domain.Auth
{
  public class AuthService
  {
    private readonly JwtSettings _jwtSettings;
    private readonly IUserRepository _userRepo;
    private readonly IUserMapper _userMapper;

    public AuthService(IUserRepository userRepo, IUserMapper userMapper, JwtSettings jwtSettings)
    {
      this._jwtSettings = jwtSettings;
      this._userRepo = userRepo;
      this._userMapper = userMapper;
    }

    public async Task<string> AuthenticateUser(UserLoginRequestDto loginRequestDto)
    {
      if (loginRequestDto.Email == null || loginRequestDto.Password == null)
      {
        throw new BusinessRuleValidationException("Email and/or password can not be null.");
      }

      var user = await this._userRepo.GetUserByEmailAsync(loginRequestDto.Email);
      if (!user.VerifyPassword(loginRequestDto.Password))
      {
        return null;
      }
      var userDto = this._userMapper.ToDto(user);
      return this.GenerateJwtToken(userDto);
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

