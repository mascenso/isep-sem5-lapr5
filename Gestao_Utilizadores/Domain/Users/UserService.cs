using System.Threading.Tasks;
using UserManagement.Domain.Shared;
using UserManagement.Mappers;
using System.Collections.Generic;
using System.Linq;


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
        throw new UserAlreadyRegisteredException($"User with email {dto.Email} already exists.");
      }
      var user = User.FromRequestDto(dto);

      await this._repo.AddAsync(user);
      await this._unitOfWork.CommitAsync();

      return this._userMapper.ToDto(user);
    }

    public async Task<UserDto> CreateSystemUser(CreateUserRequestDto requestDto)
    {
      var foundUser = await this.FindUserByEmail(requestDto.Email);
      if (foundUser != null)
      {
        throw new BusinessRuleValidationException($"User with Email {requestDto.Email} already exists.");
      }

      var user = User.CreateSystemUser(requestDto);
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
          user.Active, user.TaxPayerNumber, user.MechanographicNumber, user.PhoneNumber);
    }

    public async Task<IEnumerable<UserDto>> GetInactiveUsersAsync()
    {
        var inactiveUsers = await _repo.GetInactiveUsersAsync();
        var inactiveUserDtos = inactiveUsers.Select(user =>
            new UserDto(user.Id.AsGuid(), user.Email.Value, user.FirstName, user.LastName, user.Role.ToString(),
                user.Active, user.TaxPayerNumber, user.MechanographicNumber, user.PhoneNumber)
        );

        return inactiveUserDtos.ToList();
    }


    public async Task<UserDto> PatchUserData(UserId userId, UpdateUserRequestDto patchDto)
    {
      var existingUser = await this._repo.GetByIdAsync(userId);

      if (existingUser == null)
      {
        throw new NotFoundException($"User with ID {userId} not found.");
      }

  /*
      if (!existingUser.IsActive())
      {
        throw new BusinessRuleValidationException($"Can't patch a disabled user.");
      }
      */

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

      if ("admin@email.pt".Equals(existingUser.Email.Value))
      {
        throw new BusinessRuleValidationException($"Nice try ;)");
      }

      _repo.Remove(existingUser);
      await _unitOfWork.CommitAsync();
    }

    public async Task<UserDto> FindUserByEmail(string email)
    {
      var user = await this._repo.GetUserByEmailAsync(email);
      return user == null ? null : this._userMapper.ToDto(user);
    }

  }

}

