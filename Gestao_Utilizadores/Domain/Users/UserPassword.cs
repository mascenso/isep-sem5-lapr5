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

    private readonly string _value;
    private readonly bool _isHashed;

    private UserPassword() { }

    public UserPassword(string value, bool isHashed = false)
    {
      this._value = value;
      this._isHashed = isHashed;
    }

    public bool VerifyPassword(string password)
    {
      var salt = RandomNumberGenerator.GetBytes(KEY_SIZE);
      if (!this._isHashed)
      {
        return this._value.Equals(password);
      }
      var hashToCompare = Rfc2898DeriveBytes.Pbkdf2(password, salt, ITERATIONS, HASH_ALGORITHM, KEY_SIZE);
      return CryptographicOperations.FixedTimeEquals(hashToCompare, Convert.FromHexString(this._value));
    }

    // add validation rules (password length etc)

  }
}
