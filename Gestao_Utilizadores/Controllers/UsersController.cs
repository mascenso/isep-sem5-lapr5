using System;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using UserManagement.Domain.Auth;
using UserManagement.Domain.Shared;
using UserManagement.Domain.Users;

namespace UserManagement.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
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
        [HttpPost]
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
        }

        // GET: api/users/5
        [HttpGet("{id}")]
        public async Task<ActionResult<UserDto>> GetUserById(Guid id)
        {
          var user = await _userService.FindUserById(new UserId(id));
          if (user == null)
          {
            return NotFound();
          }

          return user;
        }

        // DELETE: api/Users/5
        [HttpDelete("{id}")]
        public async Task<ActionResult> DeleteUser(Guid id)
        {
          try
          {
            await _userService.DeleteUser(new UserId(id));
            return NoContent(); // 204 No Content
          }
          catch (NotFoundException)
          {
            return NotFound(); // 404 Not Found
          }
        }

        // PATCH: api/Users/5
        [HttpPatch("{id}")]
        public async Task<ActionResult<UserDto>> UpdateUser(Guid id, [FromBody] UpdateUserRequestDto patchDto)
        {
          try
          {
            var updatedUser = await _userService.UpdateUser(new UserId(id), patchDto);
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

        [HttpPost("login")]
        public async Task<ActionResult<string>> Login([FromBody] UserLoginRequestDto loginDto)
        {
          try
          {
            // Validate loginDto and check if user exists
            var token = await _authService.AuthenticateUser(loginDto);

            if (token == null)
            {
              // Invalid credentials
              return Unauthorized(new { Message = "Invalid email or password." });
            }
            // Login successful, generate JWT token
            return Ok(new { Token = token });
          }
          catch (BusinessRuleValidationException e)
          {
            return BadRequest(new { Message = e.Message });
          }
        }

    }
}
