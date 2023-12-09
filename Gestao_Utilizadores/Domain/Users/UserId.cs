using System;
using UserManagement.Domain.Shared;
using Newtonsoft.Json;

namespace UserManagement.Domain.Users
{
  public class UserId : EntityId
  {
    [JsonConstructor]
    public UserId(Guid value) : base(value)
    {
    }

    public UserId(String value) : base(value)
    {
    }

    override
    protected Object createFromString(String text)
    {
      return new Guid(text);
    }

    override
    public String AsString()
    {
      Guid obj = (Guid)base.ObjValue;
      return obj.ToString();
    }


    public Guid AsGuid()
    {
      return (Guid)base.ObjValue;
    }
  }
}
