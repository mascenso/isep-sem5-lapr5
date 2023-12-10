using UserManagement.Domain.Products;
using UserManagement.Infrastructure.Shared;

namespace UserManagement.Infrastructure.Products
{
    public class ProductRepository : BaseRepository<Product, ProductId>,IProductRepository
    {
        public ProductRepository(UserManagementDbContext context):base(context.Products)
        {
           
        }
    }
}
