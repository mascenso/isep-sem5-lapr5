using UserManagement.Domain.Categories;
using UserManagement.Infrastructure.Shared;

namespace UserManagement.Infrastructure.Categories
{
    public class CategoryRepository : BaseRepository<Category, CategoryId>, ICategoryRepository
    {
    
        public CategoryRepository(UserManagementDbContext context):base(context.Categories)
        {
           
        }


    }
}
