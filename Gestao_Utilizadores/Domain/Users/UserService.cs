using System;
using System.Threading.Tasks;
using Microsoft.AspNetCore.JsonPatch;
using UserManagement.Domain.Shared;

namespace UserManagement.Domain.Users
{
  public class UserService
  {
    private readonly IUnitOfWork _unitOfWork;
    private readonly IUserRepository _repo;

    public UserService(IUnitOfWork unitOfWork, IUserRepository repo)
    {
      this._unitOfWork = unitOfWork;
      this._repo = repo;
    }

    public async Task<UserDto> CreateUser(CreateUserRequestDto dto)
    {
      var user = User.FromRequestDto(dto);

      await this._repo.AddAsync(user);

      await this._unitOfWork.CommitAsync();

      return new UserDto(user.Id.AsGuid(), user.Email.ToString(), user.FirstName, user.LastName, user.Role.ToString(),
        user.Active);
    }

    public async Task<UserDto> FindUserById(UserId userId)
    {
      var user = await this._repo.GetByIdAsync(userId);
      return user == null
        ? null
        : new UserDto(user.Id.AsGuid(), user.Email.ToString(), user.FirstName, user.LastName, user.Role.ToString(),
          user.Active);
    }

    public async Task<UserDto> UpdateUser(UserId userId, UpdateUserRequestDto patchDto)
    {
      var existingUser = await this._repo.GetByIdAsync(userId);

      if (existingUser == null)
      {
        return null;
      }

      // Update user properties based on the patched DTO
      existingUser.PatchUser(patchDto);

      await _unitOfWork.CommitAsync();

      return new UserDto(existingUser.Id.AsGuid(), existingUser.Email.Value, existingUser.FirstName,
        existingUser.LastName, existingUser.Role.ToString(), existingUser.Active);
    }

    public async Task DeleteUser(UserId userId)
    {
      var existingUser = await _repo.GetByIdAsync(userId);

      if (existingUser == null)
      {
        throw new NotFoundException($"User with ID {userId} not found.");
      }

      _repo.Remove(existingUser);
      await _unitOfWork.CommitAsync();
    }
  }

}

