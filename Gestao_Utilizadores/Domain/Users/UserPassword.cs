using System;
using System.Security.Cryptography;
using System.Security.Policy;
using System.Text;
using UserManagement.Domain.Shared;


namespace UserManagement.Domain.Users
{
  public class UserPassword : IValueObject
  {
    const int KeySize = 64;
    const int Iterations = 350000;
    HashAlgorithmName HashAlgorithm = HashAlgorithmName.SHA512;

    public string Value { get; private set; }
    public bool IsHashed { get; private set; }

    private UserPassword() { }

    public UserPassword(string value, bool IsHashed = false)
    {
      this.Value = value;
      this.IsHashed = IsHashed;
    }

    public bool VerifyPassword(string password)
    {
      byte[] Salt = RandomNumberGenerator.GetBytes(KeySize);
      byte[] hashToCompare;
      if (this.IsHashed)
      {
        hashToCompare = Rfc2898DeriveBytes.Pbkdf2(password, Salt, Iterations, HashAlgorithm, KeySize);
        return CryptographicOperations.FixedTimeEquals(hashToCompare, Convert.FromHexString(this.Value));
      }
      return this.Value.Equals(password);
    }

    // add validation rules (password length etc)

  }
}
