using System;
using System.Threading.Tasks;
using Moq;
using NUnit.Framework;
using NUnit.Framework.Legacy;
using UserManagement.Domain.Users;
using UserManagement.Domain.Shared;
using UserManagement.Mappers;

namespace UserManagement.Tests.Domain.Users
{
    [TestFixture]
    public class UserServiceTest
    {

        private Mock<IUnitOfWork> unitOfWorkMock;
        private Mock<IUserRepository> userRepositoryMock;
        private IUserMapper userMapper;
        private UserService userService;


        [SetUp]
        public void Setup()
        {
          // Initialize mock objects before each test
          unitOfWorkMock = new Mock<IUnitOfWork>();
          userRepositoryMock = new Mock<IUserRepository>();
          userMapper = new UserMapperImpl();

          userService = new UserService(unitOfWorkMock.Object, userRepositoryMock.Object, userMapper);
        }

        [TearDown]
        public void TearDown()
        {
          // Clean up after each test (if needed)
          unitOfWorkMock = null;
          userRepositoryMock = null;
          userMapper = null;
          userService = null;
        }

        [Test]
        public async Task CreateUser_WhenUserDoesNotExist_ShouldCreateUser()
        {
            // given
            var createUserRequestDto = new CreateUserRequestDto(
              "manel-maquina@email.pt",
              "password",
              "Manel",
              "da Maquina",
              "USER",
              true
            );

            var user = User.FromRequestDto(createUserRequestDto);

            userRepositoryMock.Setup(repo => repo.GetUserByEmailAsync(It.IsAny<string>())).ReturnsAsync((User)null);
            userRepositoryMock.Setup(repo => repo.AddAsync(It.IsAny<User>())).ReturnsAsync(user);
            unitOfWorkMock.Setup(unitOfWork => unitOfWork.CommitAsync()).ReturnsAsync(1);

            // when
            var result = await userService.CreateUser(createUserRequestDto);

            // then
            userRepositoryMock.Verify(repo => repo.AddAsync(It.IsAny<User>()), Times.Once);
            unitOfWorkMock.Verify(unitOfWork => unitOfWork.CommitAsync(), Times.Once);
            ClassicAssert.IsNotNull(result);
            ClassicAssert.AreEqual(createUserRequestDto.Email, result.Email);
            ClassicAssert.AreEqual(createUserRequestDto.FirstName, result.FirstName);
            ClassicAssert.AreEqual(createUserRequestDto.LastName, result.LastName);
            ClassicAssert.AreEqual(createUserRequestDto.Role, result.Role);
            ClassicAssert.AreEqual(createUserRequestDto.Active, result.Active);
        }

        [Test]
        public async Task CreateUser_WhenUserExists_ShouldThrowException()
        {
            // given
            var createUserRequestDto = new CreateUserRequestDto(
                "manel-maquina@email.pt",
                "password",
                "Manel",
                "da Maquina",
                "USER",
                true
            );
            var existingUser = User.FromRequestDto(createUserRequestDto);

            userRepositoryMock.Setup(repo => repo.GetUserByEmailAsync(It.IsAny<string>())).ReturnsAsync(existingUser);

          // then
          Assert.ThrowsAsync<UserAlreadyRegisteredException>(async () => await userService.CreateUser(createUserRequestDto));
        }

        [Test]
        public async Task PatchUserData_WithValidData_ReturnsUpdatedUserDto()
        {
          // given
          var userId = new UserId(Guid.NewGuid());

          var updateUserRequestDto = new UpdateUserRequestDto
          {
            FirstName =  "Ze",
            LastName = "Tone"
          };

          var createUserRequestDto = new CreateUserRequestDto(
            "manel-maquina@email.pt",
            "password",
            "Manel",
            "da Maquina",
            "USER",
            true
          );
          var existingUser = User.FromRequestDto(createUserRequestDto);

          userRepositoryMock.Setup(repo => repo.GetByIdAsync(userId)).ReturnsAsync(existingUser);
          unitOfWorkMock.Setup(uow => uow.CommitAsync()).ReturnsAsync(1); // Adjust the return value to match the expected type

          // Act
          var result = await userService.PatchUserData(userId, updateUserRequestDto);

          // Assert
          ClassicAssert.IsNotNull(result);
          ClassicAssert.IsInstanceOf<UserDto>(result);
          ClassicAssert.AreEqual(updateUserRequestDto.FirstName, result.FirstName);
          ClassicAssert.AreEqual(updateUserRequestDto.LastName, result.LastName);
        }
    }
}
