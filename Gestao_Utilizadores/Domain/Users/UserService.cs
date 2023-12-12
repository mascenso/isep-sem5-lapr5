using System;
using System.Threading.Tasks;
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

      return new UserDto(user.Id.AsGuid(), user.Email.ToString(), user.FirstName, user.LastName, user.Role.ToString(), user.Active);
    }

    public async Task<UserDto> FindUserById(UserId userId)
    {
      var user = await this._repo.GetByIdAsync(userId);
      return user == null ? null : new UserDto(user.Id.AsGuid(), user.Email.ToString(), user.FirstName, user.LastName, user.Role.ToString(), user.Active);
    }

  }

}

