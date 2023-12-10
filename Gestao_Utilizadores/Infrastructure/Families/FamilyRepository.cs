using UserManagement.Domain.Families;
using UserManagement.Infrastructure.Shared;

namespace UserManagement.Infrastructure.Families
{
    public class FamilyRepository : BaseRepository<Family, FamilyId>, IFamilyRepository
    {
      
        public FamilyRepository(UserManagementDbContext context):base(context.Families)
        {
            
        }

    }
}
