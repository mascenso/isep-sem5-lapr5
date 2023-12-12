using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using UserManagement.Domain.Users;

namespace UserManagement.Infrastructure.Users
{
  internal class UserEntityTypeConfiguration: IEntityTypeConfiguration<User>
  {
    public void Configure(EntityTypeBuilder<User> builder)
    {
      builder.HasKey(b => b.Id);

    }
  }

}

