using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using UserManagement.Domain.Users;

namespace UserManagement.Infrastructure.Users
{
  internal class UserEntityTypeConfiguration: IEntityTypeConfiguration<User>
  {
    public void Configure(EntityTypeBuilder<User> builder)
    {
      builder.ToTable("Users"); // Set the table name

      builder.HasKey(u => u.Id); // Set the primary key

      builder.Property(u => u.Id)
        .HasColumnName("UserId"); // Map the column name for the primary key

      builder.OwnsOne(u => u.Email, email =>
      {
        email.Property(e => e.Value)
          .HasColumnName("Email")
          .IsRequired();  // Map the column name for the email property

        email.HasIndex(e => e.Value)
          .IsUnique(); // Add a unique constraint to the email property
      });

      builder.OwnsOne(u => u.Password, password =>
      {
        password.Property(p => p.Value)
          .HasColumnName("Password")
          .IsRequired(); // Map the column name for the password property
      });

      builder.Property(u => u.FirstName)
        .HasColumnName("FirstName"); // Map the column name for the firstName property

      builder.Property(u => u.LastName)
        .HasColumnName("LastName"); // Map the column name for the lastName property

      builder.Property(u => u.Role)
        .HasColumnName("Role") // Map the column name for the role property
        .HasConversion<string>()
        .IsRequired();

      builder.Property(u => u.Active)
        .HasColumnName("Active"); // Map the column name for the active property

    }
  }

}

