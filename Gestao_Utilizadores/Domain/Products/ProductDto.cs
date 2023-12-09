using System;
using UserManagement.Domain.Categories;

namespace UserManagement.Domain.Products
{
    public class ProductDto
    {
        public Guid Id { get; set; }
        public string Description { get;  set; }

        public CategoryId CategoryId { get;  set; }

        public ProductDto(Guid Id, string description, CategoryId catId)
        {
            this.Id = Id;
            this.Description = description;
            this.CategoryId = catId;
        }
    }
}
