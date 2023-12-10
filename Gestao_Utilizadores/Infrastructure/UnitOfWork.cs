using System.Threading.Tasks;
using UserManagement.Domain.Shared;

namespace UserManagement.Infrastructure
{
    public class UnitOfWork : IUnitOfWork
    {
        private readonly UserManagementDbContext _context;

        public UnitOfWork(UserManagementDbContext context)
        {
            this._context = context;
        }

        public async Task<int> CommitAsync()
        {
            return await this._context.SaveChangesAsync();
        }
    }
}
