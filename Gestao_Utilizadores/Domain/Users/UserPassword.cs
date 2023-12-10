using System;
using System.Security.Cryptography;
using System.Security.Policy;
using System.Text;
using UserManagement.Domain.Shared;


namespace UserManagement.Domain.Users
{
  public class UserPassword : IValueObject
  {
    private const int KEY_SIZE = 64;
    private const int ITERATIONS = 350000;
    private HashAlgorithmName HASH_ALGORITHM = HashAlgorithmName.SHA512;

    public string value;
    public bool isHashed;

    private UserPassword() { }

    public UserPassword(string value, bool isHashed = false)
    {
      this.value = value;
      this.isHashed = isHashed;
    }

    public bool VerifyPassword(string password)
    {
      byte[] salt = RandomNumberGenerator.GetBytes(KEY_SIZE);
      byte[] hashToCompare;
      if (this.isHashed)
      {
        hashToCompare = Rfc2898DeriveBytes.Pbkdf2(password, salt, ITERATIONS, HASH_ALGORITHM, KEY_SIZE);
        return CryptographicOperations.FixedTimeEquals(hashToCompare, Convert.FromHexString(this.value));
      }
      return this.value.Equals(password);
    }

    // add validation rules (password length etc)

  }
}
