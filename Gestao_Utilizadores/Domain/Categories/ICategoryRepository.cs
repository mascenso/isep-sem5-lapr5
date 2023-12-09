
using UserManagement.Domain.Shared;

namespace UserManagement.Domain.Categories
{
    public interface ICategoryRepository: IRepository<Category, CategoryId>
    {
    }
}
