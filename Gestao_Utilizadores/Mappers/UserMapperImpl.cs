using UserManagement.Domain.Users;

namespace UserManagement.Mappers
{
  public class UserMapperImpl: IUserMapper
  {
    public UserDto ToDto(User user)
    {
      return new UserDto(
          user.Id.AsGuid(),
          user.Email.Value,
          user.FirstName,
          user.LastName,
          user.Role.ToString(),
          user.Active,
          user.TaxPayerNumber,
          user.MechanographicNumber,
          user.PhoneNumber
        );
    }
  }

}

