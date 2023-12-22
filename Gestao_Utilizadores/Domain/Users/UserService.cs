using System;
using System.Threading.Tasks;
using Microsoft.AspNetCore.JsonPatch;
using UserManagement.Domain.Auth;
using UserManagement.Domain.Shared;
using UserManagement.Mappers;

namespace UserManagement.Domain.Users
{
  public class UserService
  {
    private readonly IUnitOfWork _unitOfWork;
    private readonly IUserRepository _repo;
    private readonly IUserMapper _userMapper;

    public UserService(
      IUnitOfWork unitOfWork,
      IUserRepository repo,
      IUserMapper userMapper)
    {
      this._unitOfWork = unitOfWork;
      this._repo = repo;
      this._userMapper = userMapper;
    }

    public async Task<UserDto> CreateUser(CreateUserRequestDto dto)
    {
      var foundUser = await this.FindUserByEmail(dto.Email);
      if (foundUser != null)
      {
        throw new BusinessRuleValidationException($"User with Email {dto.Email} already exists.");
      }
      var user = User.FromRequestDto(dto);

      await this._repo.AddAsync(user);
      await this._unitOfWork.CommitAsync();

      return this._userMapper.ToDto(user);
    }

    public async Task<UserDto> FindUserById(UserId userId)
    {
      var user = await this._repo.GetByIdAsync(userId);
      return user == null
        ? null
        : new UserDto(user.Id.AsGuid(), user.Email.Value, user.FirstName, user.LastName, user.Role.ToString(),
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

      return this._userMapper.ToDto(existingUser);
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

    public async Task<UserDto> FindUserByEmail(string email)
    {
      var user = await this._repo.GetUserByEmailAsync(email);
      if (user == null)
      {
        throw new NotFoundException($"User with email {email} not found.");
      }
      return this._userMapper.ToDto(user);
    }

  }

}

