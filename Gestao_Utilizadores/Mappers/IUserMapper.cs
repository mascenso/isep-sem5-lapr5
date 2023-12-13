using UserManagement.Domain.Users;

namespace UserManagement.Mappers
{
  public interface IUserMapper
  {
    public UserDto ToDto(User user);
  }

}

