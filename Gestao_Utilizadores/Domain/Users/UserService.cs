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
      // move validations to User class
      // create user email
      if (dto.Email == null)
      {
        throw new BusinessRuleValidationException("Email can not be null!");
      }

      if (dto.Password == null)
      {
        throw new BusinessRuleValidationException("Password can not be null!");
      }

      if (dto.Role == null)
      {
        throw new BusinessRuleValidationException("Role can not be null!");
      }

      if (!Enum.TryParse(dto.Role, out UserRole userRole))
      {
        throw new BusinessRuleValidationException("Invalid role!");
      };

      // create user password

      var user = new User(
        new UserEmail(dto.Email), new UserPassword(dto.Password),
        dto.FirstName, dto.LastName, userRole,
        true
      );

      await this._repo.AddAsync(user);

      await this._unitOfWork.CommitAsync();

      return new UserDto(user.Id.AsGuid(), user.Email, user.FirstName, user.LastName, user.Role, user.Active);
    }

    public async Task<UserDto> FindUserById(UserId userId)
    {
      var user = await this._repo.GetByIdAsync(userId);
      return user == null ? null : new UserDto(user.Id.AsGuid(), user.Email, user.FirstName, user.LastName, user.Role, user.Active);
    }

  }

}

