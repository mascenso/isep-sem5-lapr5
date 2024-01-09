using System;
using System.IdentityModel.Tokens.Jwt;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Authorization;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using UserManagement.Domain.Auth;
using UserManagement.Domain.Shared;
using UserManagement.Domain.Users;
using System.Collections.Generic;
using System.Linq;
using System.Diagnostics;


namespace UserManagement.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    [Authorize]
    public class UsersController : ControllerBase
    {
      private readonly UserService _userService;
      private readonly AuthService _authService;

        public UsersController(UserService service, AuthService authService)
        {
            _userService = service;
            _authService = authService;
        }

        // POST: api/Users
        // To protect from overposting attacks, see https://go.microsoft.com/fwlink/?linkid=2123754
        [HttpPost("register")]
        [AllowAnonymous]
        public async Task<ActionResult<UserDto>> RegisterUser(CreateUserRequestDto userDto)
        {
          try
          {
            var responseDto = await _userService.CreateUser(userDto);
            var token = this._authService.GenerateJwtToken(responseDto);
            // Include token in the response headers
            Response.Headers.Append("Authorization", "Bearer " + token);

            return CreatedAtAction(nameof(GetUserById), new { id = responseDto.Id }, responseDto);
          }
          catch (BusinessRuleValidationException e)
          {
            return BadRequest(new { Message = e.Message });
          }
          catch (UserAlreadyRegisteredException e)
          {
            return Conflict(new { Message = e.Message });
          }
        }

        // POST: api/Users/register-system-user
        // To protect from overposting attacks, see https://go.microsoft.com/fwlink/?linkid=2123754
        [Authorize(Roles = "ADMINISTRATOR")]
        [HttpPost("register-system-user")]
        public async Task<ActionResult<UserDto>> RegisterSystemUser(CreateUserRequestDto userDto)
        {
          try
          {
            var responseDto = await _userService.CreateSystemUser(userDto);
            var token = this._authService.GenerateJwtToken(responseDto);
            // Include token in the response headers
            Response.Headers.Append("Authorization", "Bearer " + token);

            return CreatedAtAction(nameof(GetUserById), new { id = responseDto.Id }, responseDto);
          }
          catch (BusinessRuleValidationException e)
          {
            return BadRequest(new { Message = e.Message });
          }
        }

        // GET: api/users/5
        [HttpGet("{id}")]
        [AllowAnonymous]
        public async Task<ActionResult<UserDto>> GetUserById(Guid id)
        {
          var user = await _userService.FindUserById(new UserId(id));
          if (user == null)
          {
            return NotFound();
          }

          return user;
        }

        [HttpGet("user")]
        public async Task<ActionResult<UserDto>> GetUser()
        {
          try
          {
            var userIdFromClaim = User.FindFirst(JwtRegisteredClaimNames.Sub)?.Value;
            var userDto = await _userService.FindUserById(new UserId(userIdFromClaim));
            return Ok(userDto); // 200 OK
          }
          catch (NotFoundException)
          {
            return NotFound(); // 404 Not Found
          }
          catch (BusinessRuleValidationException e)
          {
            return BadRequest(new { Message = e.Message }); // 400 Bad Request
          }

        }

        [HttpGet("inactive")]
        [AllowAnonymous]
        public async Task<ActionResult<IEnumerable<UserDto>>> GetInactiveUsers()
        {
            var inactiveUsers = await _userService.GetInactiveUsersAsync();

            if (inactiveUsers == null)
            {
                return NotFound();
            }

            var inactiveUserDtos = inactiveUsers.Select(user =>
                new UserDto(user.Id, user.Email, user.FirstName, user.LastName, user.Role.ToString(), user.Active, user.TaxPayerNumber, user.MechanographicNumber, user.PhoneNumber)
            );

            return inactiveUserDtos.ToList();
        }



        // DELETE: api/Users/5
        [HttpDelete("{id}")]
        [AllowAnonymous]
        public async Task<ActionResult> DeleteUserById(Guid id)
        {
          try
          {
            //var userIdFromClaim = User.FindFirst(JwtRegisteredClaimNames.Sub)?.Value;
            await _userService.DeleteUser(new UserId(id));
            return NoContent(); // 204 No Content
          }
          catch (NotFoundException)
          {
            return NotFound(); // 404 Not Found
          }
        }
        
        [HttpDelete("delete-user")]
        public async Task<ActionResult> DeleteUser()
        {
          try
          {
            var userIdFromClaim = User.FindFirst(JwtRegisteredClaimNames.Sub)?.Value;
            await _userService.DeleteUser(new UserId(userIdFromClaim));
            return NoContent(); // 204 No Content
          }
          catch (NotFoundException)
          {
            return NotFound(); // 404 Not Found
          }
        }

        // Patch: api/users/{{id}}
        [HttpPatch("{id}")]
        [AllowAnonymous]
        public async Task<ActionResult<UserDto>> PatchUserById(Guid id, UpdateUserRequestDto patchDto)
        { 
          try
          {
            var updatedUser = await _userService.PatchUserData(new UserId(id), patchDto);
            return Ok(updatedUser); // 200 OK
          }
          catch (NotFoundException)
          {
            return NotFound(); // 404 Not Found
          }
          catch (BusinessRuleValidationException e)
          {
            return BadRequest(new { Message = e.Message }); // 400 Bad Request
          }
        }

        // PATCH: api/Users/5
        [HttpPatch("patch-user")]
        [AllowAnonymous]
        public async Task<ActionResult<UserDto>> PatchUser([FromBody] UpdateUserRequestDto patchDto)
        {
          try
          {
            var userIdFromClaim = User.FindFirst(JwtRegisteredClaimNames.Sub)?.Value;
            var updatedUser = await _userService.PatchUserData(new UserId(userIdFromClaim), patchDto);
            return Ok(updatedUser); // 200 OK
          }
          catch (NotFoundException)
          {
            return NotFound(); // 404 Not Found
          }
          catch (BusinessRuleValidationException e)
          {
            return BadRequest(new { Message = e.Message }); // 400 Bad Request
          }
        }

        [AllowAnonymous]
        [HttpPost("authenticate")]
        public async Task<ActionResult<TokenDto>> Authenticate([FromBody] UserLoginRequestDto loginDto)
        {
          try
          {
            // Validate loginDto and check if user exists
            var token = await _authService.AuthenticateUser(loginDto);
            var user = await _userService.FindUserByEmail(loginDto.Email);
            var userApproved = user.Active;

            if (token == null)
            {
              // Invalid credentials
              return Unauthorized(new { Message = "Invalid email or password." });
            }
            
            if (!userApproved) {
              return Unauthorized(new { Message = "User waiting approval." });
            }
            
            
            // Login successful, generate JWT token
            // Include token in the response headers
            Response.Headers.Append("Authorization", "Bearer " + token.AccessToken);
            return Ok(token);
          }
          catch (BusinessRuleValidationException e)
          {
            return BadRequest(new { Message = e.Message });
          }
        }

        [HttpGet("validate-token")]
        public async Task<ActionResult<TokenValidationResponseDto>> ValidateTokenWithRole([FromQuery] string requiredRole)
        {
          // Check if the user has the required role
          if (User.IsInRole(requiredRole))
          {
            // If the execution reaches here, it means the user has the required role.
            // You can perform additional actions or return a success response.
            return Ok( new TokenValidationResponseDto(
              $"Token validation successful. User has the required role: {requiredRole}"));
          }

          // If the user doesn't have the required role, return a 403 Forbidden response
          return Forbid();
        }

    }
}
