using System;
using System.ComponentModel.DataAnnotations;
using UserManagement.Domain.Shared;

namespace UserManagement.Domain.Users
{
  public class User : Entity<UserId>, IAggregateRoot
  {
    [MaxLength(100)]
    public UserEmail Email { get; private set; }

    public UserPassword Password { get; private set; }

    [MaxLength(50)]
    public string FirstName { get; private set; }

    [MaxLength(50)]
    public string LastName { get; private set; }

    public UserRole Role { get; private set; }

    public bool Active { get; private set; }

    private User()
    {
      this.Active = true;
    }

    private User(UserEmail email, UserPassword password, string firstName, string lastName, UserRole role, bool active = true)
    {
      this.Id = new UserId(Guid.NewGuid());
      this.Email = email;
      this.Password = password;
      this.FirstName = firstName;
      this.LastName = lastName;
      this.Role = role;
      this.Active = active;
    }

    public static User FromRequestDto(CreateUserRequestDto dto)
    {
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
      return new User(
        new UserEmail(dto.Email), new UserPassword(UserPassword.HashPassword(dto.Password)),
        dto.FirstName, dto.LastName, userRole,
        true
      );
    }

    public void PatchUser(UpdateUserRequestDto updateDto)
    {
      if (updateDto.FirstName != null)
      {
        this.FirstName = updateDto.FirstName;
      }

      if (updateDto.LastName != null)
      {
        this.LastName = updateDto.LastName;
      }

      if (updateDto.Email != null)
      {
        this.Email = new UserEmail(updateDto.Email);
      }
    }

    public bool VerifyPassword(string password)
    {
      return this.Password.VerifyPassword(password);
    }

    //public void ChangeDescription(string description)
    //{
    //  if (!this.Active)
    //    throw new BusinessRuleValidationException("It is not possible to change the description of an inactive user.");
    //  this.Description = description;
    //}

    //public void MarkAsInative()
    //{
    //  this.Active = false;
    //}
  }
}
