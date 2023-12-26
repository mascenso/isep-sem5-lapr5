using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.EntityFrameworkCore.Storage.ValueConversion;
using UserManagement.Domain.Auth;
using UserManagement.Infrastructure;
using UserManagement.Infrastructure.Categories;
using UserManagement.Infrastructure.Products;
using UserManagement.Infrastructure.Families;
using UserManagement.Infrastructure.Shared;
using UserManagement.Domain.Shared;
using UserManagement.Domain.Categories;
using UserManagement.Domain.Products;
using UserManagement.Domain.Families;
using UserManagement.Domain.Users;
using UserManagement.Infrastructure.Users;
using UserManagement.Mappers;

namespace UserManagement
{
    public class Startup
    {
        public Startup(IConfiguration configuration)
        {
            Configuration = configuration;
        }

        public IConfiguration Configuration { get; }

        // This method gets called by the runtime. Use this method to add services to the container.
        public void ConfigureServices(IServiceCollection services)
        {
            services.AddDbContext<UserManagementDbContext>(opt =>
                opt.UseNpgsql(Configuration.GetConnectionString("DefaultConnection"))
                .ReplaceService<IValueConverterSelector, StronglyEntityIdValueConverterSelector>());

            using (var serviceScope = services.BuildServiceProvider().GetRequiredService<IServiceScopeFactory>().CreateScope())
            {
              var dbContext = serviceScope.ServiceProvider.GetRequiredService<UserManagementDbContext>();
              dbContext.Database.Migrate(); // Apply pending migrations
            }

            ConfigureMyServices(services);


            services.AddControllers().AddNewtonsoftJson();
            services.AddCors(options =>
            {
                options.AddDefaultPolicy(builder =>
                {
                    builder.AllowAnyOrigin()
                        .AllowAnyHeader()
                        .AllowAnyMethod();
                });
            });
        }

        // This method gets called by the runtime. Use this method to configure the HTTP request pipeline.
        public void Configure(IApplicationBuilder app, IWebHostEnvironment env)
        {
            if (env.IsDevelopment())
            {
                app.UseDeveloperExceptionPage();
                app.UseCors();
            }
            else
            {
                app.UseHsts();
            }

            app.UseHttpsRedirection();

            app.UseRouting();

            // app.UseCors(); // Apply CORS after routing and before Authorization

            app.UseAuthorization();

            app.UseEndpoints(endpoints =>
            {
                endpoints.MapControllers();
            });
        }



        public void ConfigureMyServices(IServiceCollection services)
        {
            services.AddSingleton<JwtSettings>(provider =>
            {
              // Load JwtSettings from configuration or wherever it is configured
              IConfiguration configuration = provider.GetRequiredService<IConfiguration>();
              return configuration.GetSection("JwtSettings").Get<JwtSettings>();
            });

            services.AddScoped<IUserMapper, UserMapperImpl>();

            services.AddTransient<AuthService>();
            services.AddTransient<IUnitOfWork,UnitOfWork>();

            services.AddTransient<ICategoryRepository,CategoryRepository>();
            services.AddTransient<CategoryService>();

            services.AddTransient<IProductRepository,ProductRepository>();
            services.AddTransient<ProductService>();

            services.AddTransient<IFamilyRepository,FamilyRepository>();
            services.AddTransient<FamilyService>();

            services.AddTransient<IUserRepository, UserRepository>();
            services.AddTransient<UserService>();
        }
    }
}
