import { expect } from 'chai';
import * as sinon from 'sinon';

import { Response, Request, NextFunction } from 'express';

import { Container } from 'typedi';
import config from "../../config";

import IRoleService from "../services/IServices/IRoleService";

import {RoleController} from "./roleController";


describe('role controller create', function () {

	beforeEach(function() {
		let roleServiceClass = require(config.services.role.path).default;
		let roleServiceInstance: IRoleService = Container.get(roleServiceClass)
		Container.set(config.services.role.name, roleServiceInstance);

		let roleService = Container.get(config.services.role.name);

		sinon.stub(roleService, 'createRole');
    });

    it('returns json with id+name values', async function () {

        let body = { "name":'role1' };
        let req: Partial<Request> = {};

        let res: Partial<Response> = {
			json: sinon.stub()
        };

		let next: Partial<NextFunction> = () => {};

		const ctrl = Container.get(RoleController);

        await ctrl.createRole(<Request>req, <Response>res, <NextFunction>next);

		sinon.assert.called(res.json as sinon.SinonStub,
			 { "id": "42db239f-d33b-4880-8caf-747c2cf41e81","name": "role1"});      
		let a = 12;
	});
});