using UserManagement.Domain.Shared;

namespace UserManagement.Domain.Products
{
    public interface IProductRepository: IRepository<Product,ProductId>
    {
    }
}
