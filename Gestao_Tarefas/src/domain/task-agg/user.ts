import {ValueObject} from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";

interface UserProps {
  userName: string;
  userContact: number;
}

export class User extends ValueObject<UserProps>{

  private constructor(props: UserProps) {
    super(props);
  }

  public static create(userName: string, userContact: number): Result<User> {


    if (userName ==null || userContact==null ) {
      return Result.fail<User>('Contact must have userName and phone number');
    }

    if (!User.isValidName(userName)) {
      return Result.fail<User>('Invalid name.');
    }

    if (!User.isValidPhoneNumber(userContact)) {
      return Result.fail<User>('Invalid number. (9s)');
    }

    return  Result.ok<User>(new User({userName, userContact: userContact}));
  }

  get userName(): string {
    return this.props.userName;
  }

  get contactNumber(): number {
    return this.props.userContact;
  }

  private static isValidName(userName: string): boolean {
    return userName.trim().length > 0;
  }

  private static isValidPhoneNumber(userContact: number): boolean {
    const nmbr = userContact.toString();
    return nmbr.length ==9;
  }
}


