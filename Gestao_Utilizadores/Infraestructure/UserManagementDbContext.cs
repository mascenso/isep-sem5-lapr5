using Microsoft.EntityFrameworkCore;
using UserManagement.Domain.Categories;
using UserManagement.Domain.Products;
using UserManagement.Domain.Families;
using UserManagement.Infrastructure.Categories;
using UserManagement.Infrastructure.Products;
using UserManagement.Domain.Users;

namespace UserManagement.Infrastructure
{
    public class UserManagementDbContext : DbContext
    {
        public DbSet<Category> Categories { get; set; }

        public DbSet<Product> Products { get; set; }

        public DbSet<Family> Families { get; set; }

        public UserManagementDbContext(DbContextOptions options) : base(options)
        {

        }

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            modelBuilder.ApplyConfiguration(new CategoryEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new ProductEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new FamilyEntityTypeConfiguration());
        }
        public DbSet<UserManagement.Domain.Users.UserDto> UserDto { get; set; }
    }
}
