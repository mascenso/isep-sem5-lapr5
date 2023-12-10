using System;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using UserManagement.Domain.Shared;
using UserManagement.Domain.Users;

namespace UserManagement.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class UsersController : ControllerBase
    {
      private readonly UserService _service;

        public UsersController(UserService service)
        {
            _service = service;
        }

        // POST: api/Users
        // To protect from overposting attacks, see https://go.microsoft.com/fwlink/?linkid=2123754
        [HttpPost]
        public async Task<ActionResult<UserDto>> CreateUser(CreateUserRequestDto userDto)
        {
          try
          {
            var responseDto = await _service.CreateUser(userDto);
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
          var user = await _service.FindUserById(new UserId(id));
          if (user == null)
          {
            return NotFound();
          }

          return user;
        }

    }
}
