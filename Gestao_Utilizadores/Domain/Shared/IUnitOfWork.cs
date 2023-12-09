using System.Threading.Tasks;

namespace UserManagement.Domain.Shared
{
    public interface IUnitOfWork
    {
        Task<int> CommitAsync();
    }
}
