using System;
using System.Security.Cryptography;
using System.Security.Policy;
using System.Text;
using UserManagement.Domain.Shared;


namespace UserManagement.Domain.Users
{
  public class UserPassword : IValueObject
  {
    private static int KEY_SIZE = 64;
    private static int ITERATIONS = 350000;
    private static HashAlgorithmName HASH_ALGORITHM = HashAlgorithmName.SHA512;

    public string Value { get; private set; }
    public bool IsHashed { get; private set; }

    private UserPassword() { }

    public UserPassword(string value, bool isHashed = false)
    {
      this.Value = value;
      this.IsHashed = isHashed;
    }

    public bool VerifyPassword(string password)
    {
      var salt = RandomNumberGenerator.GetBytes(KEY_SIZE);
      if (!this.IsHashed)
      {
        return this.Value.Equals(password);
      }
      var hashToCompare = Rfc2898DeriveBytes.Pbkdf2(password, salt, ITERATIONS, HASH_ALGORITHM, KEY_SIZE);
      return CryptographicOperations.FixedTimeEquals(hashToCompare, Convert.FromHexString(this.Value));
    }

    public static string HashPassword(string password)
    {
      var salt = RandomNumberGenerator.GetBytes(KEY_SIZE);
      return Convert.ToHexString(Rfc2898DeriveBytes.Pbkdf2(password, salt, ITERATIONS, HASH_ALGORITHM, KEY_SIZE));
    }

    // add validation rules (password length etc)

  }
}
