using Microsoft.EntityFrameworkCore;
using UserManagement.Domain.Users;
using UserManagement.Infrastructure.Users;

namespace UserManagement.Infrastructure
{
    public class UserManagementDbContext : DbContext
    {
        public DbSet<User> Users { get; set; }

        public UserManagementDbContext(DbContextOptions options) : base(options)
        {

        }

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            modelBuilder.ApplyConfiguration(new UserEntityTypeConfiguration());
        }
    }
}
